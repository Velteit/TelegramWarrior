// vim: noai:ts=2:sw=2

module TelegramWarrrior.EntryPoint

open System
open Funogram.Api
open Funogram.Types
open Funogram.Telegram.Bot
open Funogram.Telegram.Api
open Funogram.Telegram.Types
open Thoth.Json.Net
open FSharpPlus
open TelegramWarrrior.Utils
open TelegramWarrrior.Model
open System.Text.RegularExpressions

let token = "763014420:AAG2BE6MOQR5T6l3n5gwutDOfEZmyGCHUIw"

let tee f a =
  f a
  a

let const_ a b =
  a

let getTasks () =
  let mutable tasks : Task list = []
  let mutable hasResult: bool = true
  Process.run
    "task"
    ["export"]
    [(fun _ args ->
        if (not << String.IsNullOrEmpty) args.Data then Decode.fromString Task.Decoder args.Data |> (function | Ok(task) -> tasks <- task::tasks | Error(e) -> printf "%s" e))]
    (fun ex ->
        // printfn "%s" ex.Message
        hasResult <- false)
  if hasResult then Some(tasks) else None

let onHello context =
  monad' {
    let! message = context.Update.Message
    let! name = message.Chat.FirstName

    sprintf "Hello, %s!" name
    |> sendMessage message.Chat.Id
    |> api context.Config
    |> Async.Ignore
    |> Async.Start
  }
  |> ignore

let markdown (task: Task) =
  sprintf
    """*Id*: %d
*Urgency*: %.2f
*Priority*: %s
*Description*: %s
*Project*: %s
*Tags*: \[%s]
"""
    (task.Id)
    (task.Urgency)
    (task.Priority |> Option.map (string) |> Option.defaultValue "")
    (task.Description |> Option.defaultValue "")
    (task.Project |> Option.defaultValue "")
    (String.Join(", ", task.Tags))

let onStart context =
  ()

let onHelp context =
  ()

let onTask context =
  monad' {
    let! message = context.Update.Message;
    let! text = message.Text;
    ()
  }
  |> ignore
let tagKey (tag: string) =
  {
    Text = tag
    CallbackData = Some("/list +" + tag)
    Url = None
    CallbackGame = None
    SwitchInlineQuery = None
    SwitchInlineQueryCurrentChat = None
    LoginUrl = None
    Pay = None
  }
let actionButtons task = [
  {
    Text = "Complete"
    // TODO
    CallbackData = Some("/help")
    Url = None
    CallbackGame = None
    SwitchInlineQuery = None
    SwitchInlineQueryCurrentChat = None
    LoginUrl = None
    Pay = None
  }
  {
    Text = "Delete"
    // TODO
    CallbackData = Some("/help")
    Url = None
    CallbackGame = None
    SwitchInlineQuery = None
    SwitchInlineQueryCurrentChat = None
    LoginUrl = None
    Pay = None
  }
]

let taskKeyboard task = [
  task.Tags |> List.map tagKey |> List.toSeq
  (actionButtons task |> List.toSeq)
]

let onTaskList (arguments: Map<string, string list>) chatId botConfig =

  monad' {
    let! tasks = getTasks()
    // let user = context.Me.Id;

    let tagsOpt =
      arguments
      |> Map.tryFind "tags"
      |> Option.map (fun tags -> tags |> List.map (fun tag -> tag.Substring(1)))

    // TODO lenses
    tasks
    |> List.where (fun t -> t.Status |> Option.map (fun s -> s = Status.Pending) |> Option.defaultValue false)
    |> (fun list ->
      tagsOpt
      |> Option.map (fun tags ->
        list
        |> List.where (fun task -> task.Tags |> List.exists (fun t -> List.contains t tags))
      )
      |> Option.defaultValue list
    )
    |> List.sortBy (fun t -> t.Id)
    |> List.iter (
      fun task ->
        task
        |> markdown
        |> (fun text -> sendMessageBase (ChatId.Int(chatId)) text (Some ParseMode.Markdown) None None None (Some(Markup.InlineKeyboardMarkup({ InlineKeyboard = taskKeyboard task }))))
        |> api botConfig
        |> Async.Ignore
        |> Async.Start
      )
  }
  |> ignore

let groups (m: Match) =
  [|for group in m.Groups -> group|]

let success (group: Group) =
  group.Success

let captureGetValue (c: Capture) =
  c.Value

let group (group: Group) : string * string list =
  (group.Name, [for c in group.Captures -> c.Value])

let regexCmd (pattern: Regex) (handler: Map<string, string list> -> int64 -> BotConfig -> unit) (context: UpdateContext) =
  context.Update.Message
  |> Option.bind (fun message ->
    getTextForCommand context.Me message.Text
    |> Option.map (fun cmd -> pattern.Matches cmd)
    |> Option.map (fun matches -> [for g in (matches |> Seq.collect (fun m -> m.Groups)) -> (g.Name, [for c in g.Captures -> c.Value])])
    |> Option.map (fun groups -> groups |> Map.ofList)
    |> Option.map (fun m -> handler m message.Chat.Id context.Config)
  )
  |> Option.isSome
  |> not

let regexQuery (pattern: Regex) (handler: Map<string, string list> -> int64 -> BotConfig -> unit) (context: UpdateContext) =
  context.Update.CallbackQuery
  |> Option.map (fun query ->
    answerCallbackQueryBase (Some(query.Id)) None (Some(false)) None None
    |> api context.Config
    |> (Async.Ignore >> Async.Start)

    query.Data
    |> Option.map (fun cmd -> pattern.Matches cmd)
    |> Option.map (fun matches -> [for g in (matches |> Seq.collect (fun m -> m.Groups)) -> (g.Name, [for c in g.Captures -> c.Value])])
    |> Option.map (fun groups -> groups |> Map.ofList)
    |> Option.map (fun m -> query.Message |> Option.map(fun msg ->  handler m msg.Chat.Id context.Config))
  )
  |> Option.isSome
  |> not


let listRegex = Regex("/list( ?(?<tags>\\+[a-z]*) ?)+( ?due:(?<due>[0-9][0-9]))?", RegexOptions.Compiled)
let onUpdate (context: UpdateContext) =
  context.Update.Message
  |> Option.bind (fun msg -> msg.Text)
  |> Option.iter (fun txt -> printfn "Message Text: %s" txt)

  context.Update.CallbackQuery
  |> Option.bind (fun msg -> msg.Message)
  |> Option.bind (fun msg -> msg.Text)
  |> Option.iter (fun txt -> printfn "Callback Text: %s" txt)

  processCommands context [
    regexQuery listRegex onTaskList
    cmd "/start" onStart
    cmd "/help" onHelp
    regexCmd listRegex onTaskList
    cmd "/list" (fun ctx -> ctx.Update.Message |> Option.iter (fun msg -> onTaskList Map.empty msg.Chat.Id ctx.Config))
  ]
  |> ignore


[<EntryPoint>]
let main argv =
  printfn "Starting bot"
  startBot { defaultConfig with Token = token } (onUpdate) None
  |> Async.RunSynchronously
  |> ignore

  0 // return an integer exit code
