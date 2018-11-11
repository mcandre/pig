import Development.Shake
import Development.Shake.FilePath
import System.Directory as Dir

main :: IO ()
main = do
    let tarball = "dist/pig-0.0.1.tar.gz"
    homeDir <- Dir.getHomeDirectory

    shakeArgs shakeOptions{ shakeFiles="dist" } $ do
        want ["dist/bin/pig" <.> exe]

        "dist/bin/pig" <.> exe %> \out ->
            cmd_ "cabal" "install" "--bindir" "dist/bin"

        phony "hlint" $
            cmd_ "hlint" "."

        phony "lacheck" $
            mapM_ (cmd_ . ("lacheck " ++)) =<< getDirectoryFiles "." ["//*.tex"]

        phony "lint" $
            need ["hlint", "lacheck"]

        phony "test" $ do
            need ["dist/bin/pig" <.> exe]
            cmd_ ("dist/bin/pig" <.> exe)

        phony "install" $
            cmd_ "cabal" "install"

        phony "uninstall" $ do
            cmd_ "ghc-pkg" "unregister" "--force" "pig"
            removeFilesAfter homeDir ["/.cabal/bin/pig" <.> exe]

        phony "build" $
            cmd_ "cabal" "build"

        phony "haddock" $
            cmd_ "cabal" "haddock"

        tarball %> \_ -> do
            need ["build", "haddock"]
            cmd_ "cabal" "sdist"

        phony "sdist" $
            need [tarball]

        phony "publish" $ do
            need ["sdist"]
            cmd_ "cabal" "upload" tarball

        phony "clean" $
            cmd_ "cabal" "clean"
