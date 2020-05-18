// vim: noai:ts=2:sw=2

module TelegramWarrrior.Model

open System
open FSharpPlus
open Thoth.Json.Net
open TelegramWarrrior.Json.Net

type Prioriy =
  | H
  | M
  | L
  static member Decoder : Decoder<Prioriy> =
    fun path value ->
      if (Decode.isString value) then
        match (Decode.asString >> String.toUpper) value with
        | "H" -> Ok Prioriy.H
        | "M" -> Ok Prioriy.M
        | "L" -> Ok Prioriy.L
        | _ -> (path, BadPrimitive("a priority", value)) |> Error
      else
        (path, BadPrimitive("a priority", value)) |> Error


type Status =
  | Pending
  | Deleted
  | Completed
  | Waiting
  | Recurring
  static member Decoder : Decoder<Status> =
    fun path value ->
      if (Decode.isString value) then
        match (Decode.asString >> String.toLower) value with
        | "pending" -> Ok Status.Pending
        | "deleted" -> Ok Status.Deleted
        | "completed" -> Ok Status.Completed
        | "waiting" -> Ok Status.Waiting
        | "recurring" -> Ok Status.Recurring
        | _ -> (path, BadPrimitive("a priority", value)) |> Error
      else
        (path, BadPrimitive("a priority", value)) |> Error

type Annotation =
  { Entry: DateTime;
    Description: string; }
  static member Decoder: Decoder<Annotation> =
    Decode.object
      (fun get ->
        { Entry = get.Required.Field "entry" (Decode.customDateTime "yyyyMMddTHHmmssK")
          Description = get.Required.Field "description" Decode.string  })

let exceptNames = ["id"; "urgency"; "uuid"; "entry"; "due"; "end"; "start"; "until"; "wait"; "modified"; "scheduled"; "project"; "description"; "mask"; "tags"; "priority"; "status"; "annotations"]

type Task =
  { Id: int32;
    Uid: Guid;
    Due: DateTime option;
    End: DateTime option;
    Start: DateTime option;
    Until: DateTime option;
    Scheduled: DateTime option;
    Wait: DateTime option;
    Modified: DateTime option;
    Entry: DateTime;
    Priority: Prioriy option;
    Project: string option;
    Tags: string list;
    Description: string option;
    Status: Status option
    Urgency: float;
    Annotations: Annotation list;
    Mask: string option;
    UDAs: (string * string) list; }
  static member Decoder : Decoder<Task> =
    Decode.object
      (fun get ->
        { Id = get.Required.Field "id" Decode.int;
          Urgency = get.Required.Field "urgency" Decode.float;
          Uid = get.Required.Field "uuid" Decode.guid;
          Entry = get.Required.Field "entry" (Decode.customDateTime "yyyyMMddTHHmmssK");
          Due = get.Optional.Field "due" (Decode.customDateTime "yyyyMMddTHHmmssK");
          End = get.Optional.Field "end" (Decode.customDateTime "yyyyMMddTHHmmssK");
          Start = get.Optional.Field "start" (Decode.customDateTime "yyyyMMddTHHmmssK");
          Until = get.Optional.Field "until" (Decode.customDateTime "yyyyMMddTHHmmssK");
          Wait = get.Optional.Field "wait" (Decode.customDateTime "yyyyMMddTHHmmssK");
          Modified = get.Optional.Field "modified" (Decode.customDateTime "yyyyMMddTHHmmssK");
          Scheduled = get.Optional.Field "scheduled" (Decode.customDateTime "yyyyMMddTHHmmssK");
          Project = get.Optional.Field "project" Decode.string;
          Description = get.Optional.Field "description" Decode.string;
          Mask = get.Optional.Field "mask" Decode.string;
          Tags = get.Optional.Field "tags" (Decode.list Decode.string)
                 |> Option.defaultValue [];
          Priority = get.Optional.Field "priority" (Prioriy.Decoder);
          Status = get.Optional.Field "status" Status.Decoder;
          Annotations = get.Optional.Field "annotations" (Decode.list Annotation.Decoder)
                        |> Option.defaultValue [];
          UDAs = get.Required.Raw (Decode.tuples exceptNames Decode.string) ;  })


