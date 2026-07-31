module Generator.JsImportTest where

import StrongPath (Dir, Path, Posix, Rel)
import qualified StrongPath as SP
import Test.Hspec
import Wasp.AppSpec.ExtImport
import Wasp.AppSpec.ExtImport.Source
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.JsImport
import qualified Wasp.Generator.SdkGenerator.JsImport as SdkGenerator
import Wasp.Generator.ServerGenerator.Common (ServerSrcDir)
import Wasp.JsImport as JI

spec_GeneratorJsImportTest :: Spec
spec_GeneratorJsImportTest = do
  let projectExtImport =
        ExtImport
          { name = ExtImportModule "test",
            source = ProjectSrcExtImportSource [SP.relfileP|folder/test.js|],
            alias = Nothing
          }
  describe "extImportToJsImport" $ do
    let pathToExtCodeDir = [SP.reldirP|src|] :: (Path Posix (Rel ServerSrcDir) (Dir GeneratedExternalCodeDir))
        pathFromImportLocationToExtCodeDir = [SP.reldirP|../|]
    it "makes a JsImport from ExtImport" $ do
      extImportToJsImport pathToExtCodeDir pathFromImportLocationToExtCodeDir projectExtImport
        `shouldBe` JI.JsImport
          { JI._kind = JI.ValueImport,
            JI._path = JI.RelativeImportPath [SP.relfileP|../src/folder/test.js|],
            JI._name = JsImportModule "test",
            JI._importAlias = Nothing
          }
    it "uses alias metadata for generated ExtImport identifiers" $ do
      getAliasedExtImportIdentifier projectExtImport {alias = Just "testAlias"}
        `shouldBe` "testAlias_ext"
    it "avoids collisions for same exported name with different aliases" $ do
      let firstImport = projectExtImport {name = ExtImportField "handler", source = ProjectSrcExtImportSource [SP.relfileP|one.js|], alias = Just "oneHandler"}
          secondImport = projectExtImport {name = ExtImportField "handler", source = ProjectSrcExtImportSource [SP.relfileP|two.js|], alias = Just "twoHandler"}
      getAliasedExtImportIdentifier <$> [firstImport, secondImport]
        `shouldBe` ["oneHandler_ext", "twoHandler_ext"]
  describe "context-specific project source paths" $ do
    it "maps Vite imports to extensionless project src paths" $ do
      extImportToJsImportFromViteExecution projectExtImport
        `shouldBe` JI.JsImport
          { JI._kind = JI.ValueImport,
            JI._path = JI.RelativeImportPath [SP.relfileP|src/folder/test|],
            JI._name = JsImportModule "test",
            JI._importAlias = Just "test_ext"
          }
    it "maps SDK imports to extensionless SDK module paths" $ do
      SdkGenerator.extImportToJsImport projectExtImport
        `shouldBe` JI.JsImport
          { JI._kind = JI.ValueImport,
            JI._path = JI.ModuleImportPath [SP.relfileP|wasp/src/folder/test|],
            JI._name = JsImportModule "test",
            JI._importAlias = Just "test_ext"
          }
  describe "extImportSourceToJsImportPath" $ do
    it "maps package sources to raw import names" $ do
      extImportSourceToJsImportPath
        (const $ JI.RawImportName "project-path")
        (PackageExtImportSource $ PackageImportSource "@skateboard/fsm" $ Just "SkateboardPage")
        `shouldBe` JI.RawImportName "@skateboard/fsm/SkateboardPage"
