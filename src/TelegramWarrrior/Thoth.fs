// vim: noai:ts=2:sw=2

module TelegramWarrrior.Json.Net

open Thoth.Json.Net
open System
open System.Globalization

module Decode =
  open Newtonsoft.Json
  open Newtonsoft.Json.Linq

  let inline isString (token: JsonValue) = not(isNull token) && token.Type = JTokenType.String
  let inline asString (token: JsonValue): string = token.Value<string>()

  let customDateTime (pattern: string) : Decoder<DateTime> =
    fun path value ->
      if (isString value) then
        match DateTime.TryParseExact(asString value, pattern, CultureInfo.InvariantCulture, DateTimeStyles.None) with
        | true, x -> x.ToUniversalTime() |> Ok
        | _ -> (path, BadPrimitive("a datetime", value)) |> Error
      else
        (path, BadPrimitive("a datetime", value)) |> Error

  let tuples (except: string list) (decoder: Decoder<'t>): Decoder<(string * 't) list> =
    fun path value ->
      let json = (value :?> JObject)
      let properties = [
        for property in json.Properties() do
          if (not (List.contains property.Name except)) then
            yield (property.Name, property.Value)
      ]

      let rec gen (result: (string * 't) list) = function
        | (key, value)::tail ->
          match decoder (sprintf "%s.%s" path key) value with
          | Ok(e) ->  gen ((key, e)::result) tail
          | _ -> gen result tail
        | [] -> result

      properties
      |> gen []
      |> Result.Ok
