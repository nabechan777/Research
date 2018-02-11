
import Distribution.MacOSX
import Distribution.Simple

main = defaultMainWithHooks $ simpleUserHooks
    { postBuild = appBundleBuildHook guiApps
    }

guiApps :: [MacApp]
guiApps = return $ MacApp
    { appName   = "Research"
    , appIcon   = Just "./resource/research.icns"
    , appPlist  = Nothing
    , resources = []
    , otherBins = []
    , appDeps   = DoNotChase
    }
