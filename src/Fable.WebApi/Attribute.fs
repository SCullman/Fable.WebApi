module Fable.WebApi

open System
open System.Net.Http.Formatting
open Newtonsoft.Json
open Djehuti.Json.Net.Converters
open System.Web.Http.Controllers

[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method, AllowMultiple = false)>]
type FableWebApiAttribute() =
    inherit System.Attribute()
    interface IControllerConfiguration with
        member __.Initialize((controllerSettings : HttpControllerSettings), _) =
            let converter = CacheConverter(converters)
            let thothFormatter =
                JsonMediaTypeFormatter
                    (SerializerSettings = JsonSerializerSettings(Converters = [| converter |]))
            controllerSettings.Formatters.Clear()
            controllerSettings.Formatters.Add thothFormatter
        