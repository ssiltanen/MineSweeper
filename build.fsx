#r "paket:
nuget FSharp.Core
nuget Fake.Core.ReleaseNotes
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Tools.Git //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO
open Fake.Tools
open Fake.Core.TargetOperators

Target.initEnvironment ()

// Git configuration (used for publishing in gh-pages branch)
// The profile where the project is posted
let gitOwner = "ssiltanen"
let gitHome = "https://github.com/" + gitOwner
// The name of the project on GitHub
let gitProjectName = "MineSweeper"
// The name of the GitHub repo subdirectory to publish slides to
let gitSubDir = ""

let deployPath = Path.getFullName "./deploy"
let tempDocsRoot = Path.getFullName "./temp/gh-pages"
let bin = Path.getFullName "./src/bin"
let obj = Path.getFullName "./src/obj"

let platformTool tool winTool =
    let tool = if Environment.isUnix then tool else winTool
    match ProcessUtils.tryFindFileOnPath tool with
    | Some t -> t
    | _ ->
        let errorMsg =
            tool + " was not found in path. " +
            "Please install it and make sure it's available from your path. " +
            "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
        failwith errorMsg

let npmTool = platformTool "npm" "npm.exe"

let runTool cmd args workingDir =
    let arguments = args |> String.split ' ' |> Arguments.OfArgs
    RawCommand (cmd, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

Target.create "Clean" (fun _ ->
    [ deployPath; tempDocsRoot; bin; obj ]
    |> Shell.cleanDirs
)

Target.create "Build" (fun _ ->
    runTool npmTool "run build" __SOURCE_DIRECTORY__
)

Target.create "Release" (fun _ ->
    Git.Repository.cloneSingleBranch "" (gitHome + "/" + gitProjectName + ".git") "gh-pages" tempDocsRoot
    let tempDocsDir = Path.combine tempDocsRoot gitSubDir
    Git.Repository.fullclean tempDocsDir
    Shell.copyRecursive deployPath tempDocsDir true |> ignore
    Git.Staging.stageAll tempDocsRoot
    Git.Commit.exec tempDocsRoot "Update static files"
    Git.Branches.push tempDocsRoot
)

"Clean"
    ==> "Build"
    ==> "Release"

Target.runOrDefaultWithArguments "Release"