
import           Control.Monad (unless)
import           System.Exit (exitFailure)
import           System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Course.ApplicativeSpec as ApplicativeSpec

main :: IO ()
main = runSuites [ ApplicativeSpec.tests ]

  -- | To be used as the `main` of Hedgehog test suites.
runSuites :: [IO Bool] -> IO ()
runSuites tests = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence tests

  unless (and results) exitFailure
