module Wasp.Cli.Command.Start.Db
  ( start,
  )
where

import Control.Monad (unless, when)
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import qualified Options.Applicative as Opt
import StrongPath (Abs, Dir, File', Path', Rel, fromRelFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (LineBuffering), Handle, hGetLine, hIsEOF, hPutStrLn, hSetBuffering, stderr)
import System.Process (CreateProcess (std_err), StdStream (CreatePipe), createProcess, proc, waitForProcess)
import Text.Printf (printf)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Common (throwIfExeIsNotAvailable)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Util.Parser (withArguments)
import Wasp.Db.Postgres (defaultPostgresDockerImageSpec)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.Db (databaseUrlEnvVarName)
import qualified Wasp.Project.Db.Dev.Postgres as Dev.Postgres
import Wasp.Project.Env (dotEnvServer)
import Wasp.Util.Docker (DockerImageName, DockerVolumeMountPath)
import qualified Wasp.Util.Network.Socket as Socket

-- | Starts a "managed" dev database, where "managed" means that
-- Wasp creates it and connects the Wasp app with it.
-- Wasp is smart while doing this so it checks which database is specified
-- in Wasp configuration and spins up a database of appropriate type.
start :: Arguments -> Command ()
start = withArguments "wasp start db" startDbArgsParser $ \args -> do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  appSpec <- analyze waspProjectDir

  throwIfCustomDbAlreadyInUse appSpec

  let (appName, _) = ASV.getApp appSpec

  case ASV.getValidDbSystem appSpec of
    AS.App.Db.SQLite -> noteSQLiteDoesntNeedStart
    AS.App.Db.PostgreSQL ->
      startPostgresDevDb
        waspProjectDir
        appName
        (dbImage args)
        (dbVolumeMountPath args)
  where
    noteSQLiteDoesntNeedStart =
      cliSendMessageC . Msg.Info $
        "Nothing to do! You are all good, you are using SQLite which doesn't need to be started."

startDbArgsParser :: Opt.Parser StartDbArgs
startDbArgsParser =
  StartDbArgs
    <$> Opt.strOption
      ( Opt.long "db-image"
          <> Opt.metavar "IMAGE"
          <> Opt.help "Docker image to use for the database"
          <> Opt.showDefault
          <> Opt.value (fst defaultPostgresDockerImageSpec)
      )
    <*> Opt.strOption
      ( Opt.long "db-volume-mount-path"
          <> Opt.metavar "PATH"
          <> Opt.help "Path inside Docker container where database files are stored"
          <> Opt.showDefault
          <> Opt.value (snd defaultPostgresDockerImageSpec)
      )

data StartDbArgs = StartDbArgs
  { dbImage :: DockerImageName,
    dbVolumeMountPath :: DockerVolumeMountPath
  }

throwIfCustomDbAlreadyInUse :: AS.AppSpec -> Command ()
throwIfCustomDbAlreadyInUse spec = do
  throwIfDbUrlInEnv
  throwIfDbUrlInServerDotEnv spec
  where
    throwIfDbUrlInEnv :: Command ()
    throwIfDbUrlInEnv = do
      dbUrl <- liftIO $ lookupEnv databaseUrlEnvVarName
      when (isJust dbUrl) $
        throwCustomDbAlreadyInUseError
          ( "Wasp has detected existing "
              <> databaseUrlEnvVarName
              <> " var in your environment.\n"
              <> "To have Wasp run the dev database for you, make sure you remove that env var first."
          )

    throwIfDbUrlInServerDotEnv :: AS.AppSpec -> Command ()
    throwIfDbUrlInServerDotEnv appSpec =
      when (isThereDbUrlInServerDotEnv appSpec) $
        throwCustomDbAlreadyInUseError
          ( printf
              ( "Wasp has detected that you have defined %s env var in your %s file.\n"
                  <> "To have Wasp run the dev database for you, make sure you remove that env var first."
              )
              databaseUrlEnvVarName
              (fromRelFile (dotEnvServer :: Path' (Rel WaspProjectDir) File'))
          )
      where
        isThereDbUrlInServerDotEnv = any ((== databaseUrlEnvVarName) . fst) . AS.devEnvVarsServer

    throwCustomDbAlreadyInUseError :: String -> Command ()
    throwCustomDbAlreadyInUseError msg =
      E.throwError $ CommandError "You are using custom database already" msg

startPostgresDevDb :: Path' Abs (Dir WaspProjectDir) -> String -> DockerImageName -> DockerVolumeMountPath -> Command ()
startPostgresDevDb waspProjectDir appName dbDockerImage dbDockerVolumeMountPath = do
  throwIfExeIsNotAvailable
    "docker"
    "To run PostgreSQL dev database, Wasp needs `docker` installed and in PATH."

  maybeAlreadyRunningPort <- liftIO $ Dev.Postgres.discoverDevDbPort waspProjectDir appName
  case maybeAlreadyRunningPort of
    Just port -> noteDbIsAlreadyRunning port
    Nothing -> startOnFirstFreePort candidatePorts
  where
    -- We scan sequentially from the default port so that behavior is predictable:
    -- a lone Wasp app on a machine with a free 5432 always gets 5432.
    candidatePorts = take numOfPortsToScan [Dev.Postgres.defaultDevPort ..]
    numOfPortsToScan = 20

    noteDbIsAlreadyRunning :: Int -> Command ()
    noteDbIsAlreadyRunning port =
      cliSendMessageC . Msg.Info $
        unlines
          [ printf "Your dev database is already running on port %d." port,
            "Connection URL, in case you might want to connect with external tools:",
            "  " <> Dev.Postgres.makeDevConnectionUrl waspProjectDir appName port
          ]

    startOnFirstFreePort :: [Int] -> Command ()
    startOnFirstFreePort [] = throwNoFreePortError
    startOnFirstFreePort (port : remainingPorts) = do
      portIsBusy <- liftIO $ checkIfPortIsBusy port
      if portIsBusy
        then do
          cliSendMessageC . Msg.Info $ printf "Port %d is busy, trying the next one." port
          startOnFirstFreePort remainingPorts
        else do
          printStartingDbInfo port
          runResult <- liftIO $ runDbDockerContainer port
          case runResult of
            -- Between our port check and docker binding the port, somebody else
            -- (e.g. another Wasp project starting in parallel) might have taken it.
            -- Docker's port bind is the final arbiter, so on that specific failure
            -- we just move on to the next port.
            DbRunFailedPortTaken -> do
              cliSendMessageC . Msg.Info $
                printf "Port %d got taken in the meantime, trying the next one." port
              startOnFirstFreePort remainingPorts
            DbRunExited ExitSuccess -> return ()
            DbRunExited (ExitFailure exitCode) ->
              E.throwError $
                CommandError "Dev database failed" $
                  printf "Running the dev database Docker container failed with exit code %d." exitCode

    checkIfPortIsBusy :: Int -> IO Bool
    checkIfPortIsBusy port = do
      -- I am checking both conditions because of Docker having virtual network on Mac which
      -- always gives precedence to native ports so checking only if we can open the port is
      -- not enough because we can open it even if Docker container is already bound to that port.
      portIsInUse <- Socket.checkIfPortIsInUse socketAddress
      if portIsInUse
        then return True
        else Socket.checkIfPortIsAcceptingConnections socketAddress
      where
        socketAddress = Socket.makeLocalHostSocketAddress $ fromIntegral port

    runDbDockerContainer :: Int -> IO DbRunResult
    runDbDockerContainer port = do
      (_, _, Just dockerStderr, dockerProcess) <-
        createProcess (proc "docker" (dockerRunArgs port)) {std_err = CreatePipe}
      sawPortAllocatedErrorRef <- newIORef False
      relayLinesAndDetectPortAllocatedError dockerStderr sawPortAllocatedErrorRef
      exitCode <- waitForProcess dockerProcess
      sawPortAllocatedError <- readIORef sawPortAllocatedErrorRef
      return $
        if sawPortAllocatedError && exitCode /= ExitSuccess
          then DbRunFailedPortTaken
          else DbRunExited exitCode

    relayLinesAndDetectPortAllocatedError :: Handle -> IORef Bool -> IO ()
    relayLinesAndDetectPortAllocatedError dockerStderr sawPortAllocatedErrorRef = do
      hSetBuffering dockerStderr LineBuffering
      relayLines
      where
        relayLines = do
          isEof <- hIsEOF dockerStderr
          unless isEof $ do
            line <- hGetLine dockerStderr
            hPutStrLn stderr line
            when (isPortAllocatedError line) $ writeIORef sawPortAllocatedErrorRef True
            relayLines

        isPortAllocatedError line = "port is already allocated" `isInfixOf` map toLower line

    -- NOTE: POSTGRES_PASSWORD, POSTGRES_USER, POSTGRES_DB below are really used by the docker image
    --   only when initializing the database -> if it already exists, they will be ignored.
    --   This is how the postgres Docker image works.
    dockerRunArgs :: Int -> [String]
    dockerRunArgs port =
      [ "run",
        "--name",
        dockerContainerName,
        "--rm",
        "--publish",
        printf "%d:5432" port,
        "--volume",
        dockerVolumeName <> ":" <> dbDockerVolumeMountPath,
        "--env",
        "POSTGRES_PASSWORD=" <> Dev.Postgres.defaultDevPass,
        "--env",
        "POSTGRES_USER=" <> Dev.Postgres.defaultDevUser,
        "--env",
        "POSTGRES_DB=" <> dbName,
        dbDockerImage
      ]

    printStartingDbInfo :: Int -> Command ()
    printStartingDbInfo port = do
      cliSendMessageC . Msg.Info $
        unlines
          [ "✨ Starting a PostgreSQL dev database (based on your Wasp config) ✨",
            "",
            "Additional info:",
            " ℹ Using Docker image: " <> dbDockerImage,
            "   with the data volume mounted at: " <> dbDockerVolumeMountPath,
            " ℹ Connection URL, in case you might want to connect with external tools:",
            "     " <> Dev.Postgres.makeDevConnectionUrl waspProjectDir appName port,
            " ℹ Database data is persisted in a Docker volume with the following name"
              <> " (useful to know if you will want to delete it at some point):",
            "     " <> dockerVolumeName
          ]
      cliSendMessageC $ Msg.Info "..."

    throwNoFreePortError :: Command ()
    throwNoFreePortError =
      E.throwError $
        CommandError
          "No free port"
          ( printf
              "Wasp can't run PostgreSQL dev database for you since all ports from %d to %d are already in use."
              Dev.Postgres.defaultDevPort
              (Dev.Postgres.defaultDevPort + numOfPortsToScan - 1)
          )

    dockerVolumeName = Dev.Postgres.makeWaspDevDbDockerVolumeName waspProjectDir appName
    dockerContainerName = Dev.Postgres.makeWaspDevDbDockerContainerName waspProjectDir appName
    dbName = Dev.Postgres.makeDevDbName waspProjectDir appName

data DbRunResult = DbRunFailedPortTaken | DbRunExited ExitCode
