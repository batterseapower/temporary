module System.IO.Temp (
    withSystemTempFile, withSystemTempDirectory,
    withTempFile, withTempDirectory,
    withTempFileName, withSystemTempFileName,
    module Distribution.Compat.TempFile,
    writeTempFile, writeSystemTempFile
  ) where

-- NB: this module was extracted directly from "Distribution/Simple/Utils.hs"
-- in a Cabal tree whose most recent commit was on Sun Oct 10 22:00:26
--
-- The files in the Distribution/Compat tree are exact copies of the corresponding
-- file in the Cabal checkout.


import Control.Monad.Catch as Exception

import Control.Monad.IO.Class
import System.Directory
import System.IO

import Distribution.Compat.TempFile


-- | Create and use a temporary directory in the system standard temporary directory.
--
-- Behaves exactly the same as 'withTempFile', except that the parent temporary directory
-- will be that returned by 'getTemporaryDirectory'.
withSystemTempFile :: (MonadIO m, MonadMask m) =>
                      String   -- ^ File name template. See 'openTempFile'.
                   -> (FilePath -> Handle -> m a) -- ^ Callback that can use the file
                   -> m a
withSystemTempFile template action = liftIO getTemporaryDirectory >>= \tmpDir -> withTempFile tmpDir template action

-- | Create and use a temporary directory in the system standard temporary directory.
--
-- Behaves exactly the same as 'withTempDirectory', except that the parent temporary directory
-- will be that returned by 'getTemporaryDirectory'.
withSystemTempDirectory :: (MonadIO m, MonadMask m) =>
                           String   -- ^ Directory name template. See 'openTempFile'.
                        -> (FilePath -> m a) -- ^ Callback that can use the directory
                        -> m a
withSystemTempDirectory template action = liftIO getTemporaryDirectory >>= \tmpDir -> withTempDirectory tmpDir template action


-- | Use a temporary filename that doesn't already exist.
--
-- Creates a new temporary file inside the given directory, making use of the
-- template. The temp file is deleted after use. For example:
--
-- > withTempFile "src" "sdist." $ \tmpFile hFile -> do ...
--
-- The @tmpFlie@ will be file in the given directory, e.g.
-- @src/sdist.342@.
withTempFile :: (MonadIO m, MonadMask m) =>
                FilePath -- ^ Temp dir to create the file in
             -> String   -- ^ File name template. See 'openTempFile'.
             -> (FilePath -> Handle -> m a) -- ^ Callback that can use the file
             -> m a
withTempFile tmpDir template action =
  Exception.bracket
    (liftIO (openTempFile tmpDir template))
    (\(name, handle) -> liftIO (hClose handle >> ignoringIOErrors (removeFile name)))
    (uncurry action)



-- | Find a temporary filename that doesn't already exist, but don't
--   do anything with it. The supplied action is responsible for
--   creating/initialising the file. After doing that, it may
--   pass on the path or handles (via the monadic result),
--   or immediately delete the file again.
--   As long as the file has not actually been created, it is not
--   guaranteed that subsequent use of the actions in this module
--   will not try to use that same name for other actions.
-- 
--   The main purpose of this function is to work with opaque file-creating
--   libraries, e.g. if you want to generate PDF files with cairo,
--   but not specify any unsafe file locations.
withTempFileName :: (MonadIO m, MonadMask m) =>
                    FilePath -- ^ Temp dir to create the file in
                 -> String   -- ^ File name template. See 'openTempFile'.
                 -> (FilePath -> m a) -- ^ Callback that can use the file name.
                 -> m a
withTempFileName tmpDir template action = do
    (filePath,handle) <- liftIO $ openTempFile tmpDir template
    liftIO $ hClose handle        -- This is a hack. We shouldn't have to
    liftIO $ removeFile filePath  -- create, then delete, then properly-create the file.
    action filePath

-- | Like 'withSystemTempFileName', but use the system directory for temporary files.
withSystemTempFileName :: (MonadIO m, MonadMask m) =>
                      String   -- ^ File name template. See 'openTempFile'.
                   -> (FilePath -> m a) -- ^ Callback that can use the file name
                   -> m a
withSystemTempFileName template action
    = liftIO getTemporaryDirectory >>= \tmpDir -> withTempFileName tmpDir template action



-- | Create and use a temporary directory.
--
-- Creates a new temporary directory inside the given directory, making use
-- of the template. The temp directory is deleted after use. For example:
--
-- > withTempDirectory "src" "sdist." $ \tmpDir -> do ...
--
-- The @tmpDir@ will be a new subdirectory of the given directory, e.g.
-- @src/sdist.342@.
withTempDirectory :: (MonadMask m, MonadIO m) =>
                     FilePath -- ^ Temp directory to create the directory in
                  -> String   -- ^ Directory name template. See 'openTempFile'.
                  -> (FilePath -> m a) -- ^ Callback that can use the directory
                  -> m a
withTempDirectory targetDir template =
  Exception.bracket
    (liftIO (createTempDirectory targetDir template))
    (liftIO . ignoringIOErrors . removeDirectoryRecursive)


-- | Create a unique new file, write (text mode) a given data string to it,
--   and close the handle again. The file will not be deleted automatically,
--   and only the current user will have permission to access the file
--   (see `openTempFile` for details).
writeTempFile :: FilePath    -- ^ Directory in which to create the file
              -> String      -- ^ File name template.
              -> String      -- ^ Data to store in the file.
              -> IO FilePath -- ^ Path to the (written and closed) file.
writeTempFile targetDir template content = Exception.bracket
    (openTempFile targetDir template)
    (\(_, handle) -> hClose handle)
    (\(filePath, handle) -> hPutStr handle content >> return filePath)

-- | Like 'writeTempFile', but use the system directory for temporary files.
writeSystemTempFile :: String      -- ^ File name template.
                    -> String      -- ^ Data to store in the file.
                    -> IO FilePath -- ^ Path to the (written and closed) file.
writeSystemTempFile template content
    = getTemporaryDirectory >>= \tmpDir -> writeTempFile tmpDir template content


ignoringIOErrors :: MonadCatch m => m () -> m ()
ignoringIOErrors ioe = ioe `Exception.catch` (\e -> const (return ()) (e :: IOError))
