module Main
  where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Function
import Data.List (intersperse, sortBy, foldl')

import System.Directory
import System.FilePath
import System.FilePath.Find

import qualified Data.Text as T

import Audio.Metadata

type Classification = ([FilePath], T.Text)

qmap :: (a -> b) -> (z, a) -> (z, b)
qmap f (a,b) = (a, f b)

try :: (a -> Maybe b) -> a -> Either a b
try f a = maybe (Left a) Right $ f a

readTags name = do
    tags <- parseFile metadata name
    return $ fmap ((,) name) tags


toFilepath = T.unpack . T.filter (/= '\NUL') . T.replace (T.singleton '/') (T.singleton '#')

makeName :: Int -> Metadata -> Maybe T.Text
makeName padding meta = do
    mArtist <- artist meta >>= listToMaybe
    mTitle <- title meta >>= listToMaybe
    let (mAlbum, mTrack) = maybe ([], []) id $ do
            mAlbum <- album meta >>= listToMaybe
            let mTrack = T.concat . map (fst . T.break ((==) '/')) . concat . maybeToList $ track meta
            return ([mAlbum], [T.justifyRight padding '0' mTrack])
    return . T.intercalate (T.pack " - ") .  concat $ [[mArtist], mAlbum, mTrack, [mTitle]] 

makePath :: Metadata -> Maybe Classification
makePath meta = do
    mArtist <- artist meta >>= listToMaybe
    name <- makeName 2 meta
    let path = [toFilepath mArtist] ++ (map toFilepath $ maybeToList (album meta >>= listToMaybe))

    return (path, name)


partition :: [(a, Either b c)] -> ([(a, b)], [(a, c)])
partition = foldr (uncurry . flip $ either left right) ([], [])
  where
    left a x ~(l,r) = ((x,a):l, r)
    right b x ~(l,r) = (l, (x,b):r)

    
main = do
    dir <- getCurrentDirectory
    files <- find ((/=) '.' . head <$> fileName) (fileType ==? RegularFile) dir
    tags <- catMaybes <$> mapM readTags files
    
    let (rejects, classifed) = partition $ map (qmap $ try makePath) tags
        group :: [(FilePath, ([FilePath], T.Text))] -> [([FilePath], [(FilePath, T.Text)])]
        group [] = []
        group ((file, (paths, name)):xs) = (paths, (file, name) : dropPaths vals) : group rest
            where 
                (vals, rest) = span ((==) paths . fst . snd) xs
                dropPaths = map (qmap snd) 
        order = group . sortBy (compare `on` (snd . snd)) $ classifed

    forM order $ \(paths, files) -> do
        let path =  foldl' (</>) "music" paths
        createDirectoryIfMissing True path
        forM files $ \(file, name) -> do
            let name' = addExtension (toFilepath name) $ takeExtension file
                newfile = path </> name'
            exists <- doesFileExist newfile
            if exists
                then putStrLn $ "\tfile exists... skipping " ++ file
                else renameFile file (path </> name')
            
        
    when (not . null $ rejects) $ () <$ do
        putStrLn "### Rejects ###"
        forM rejects (\(file, meta) -> putStrLn . (++) file . maybe [] (show . T.unpack) $ makeName 2 meta)

