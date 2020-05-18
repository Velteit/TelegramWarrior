module TelegramWarrrior.Utils

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
  else None


module Process =
  open System
  open System.Diagnostics

  let run processName (arguments: string list) (handlers: (obj -> DataReceivedEventArgs -> unit) list) (exceptionHandler: Exception -> unit) =
    let startInfo =
      ProcessStartInfo(
        RedirectStandardOutput = true,
        FileName = processName
      );

    for argument in arguments do
      startInfo.ArgumentList.Add argument

    let task = new Process(StartInfo = startInfo);

    for handler in handlers do
      task.OutputDataReceived.AddHandler(DataReceivedEventHandler (handler))

    try
      task.Start() |> ignore
    with | ex ->
      exceptionHandler ex

    task.BeginOutputReadLine();
    task.WaitForExit();
