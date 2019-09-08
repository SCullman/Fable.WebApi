module Fable.WebApi

open System
open System.Net.Http.Formatting
open Newtonsoft.Json
open Djehuti.Json.Net.Converters
open System.Web.Http.Filters

[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method, AllowMultiple = false)>]
type FableWebApiAttribute() =
    inherit ActionFilterAttribute()
    member val oldFormatters:MediaTypeFormatter list = List.Empty with get,set
    member val fableFormatter = 
        let converter = CacheConverter(converters)
        JsonMediaTypeFormatter (SerializerSettings = JsonSerializerSettings(Converters = [|converter|]))

    override __.OnActionExecuting actionContext =
        let formatters = actionContext.ControllerContext.Configuration.Formatters
        __.oldFormatters <- formatters |> Seq.toList
        formatters.Clear()
        formatters.Add __.fableFormatter
        base.OnActionExecuting actionContext
    
    override __.OnActionExecuted actionExecutedContext =
        let formatters = actionExecutedContext.ActionContext.ControllerContext.Configuration.Formatters
        formatters.Clear()
        formatters.AddRange __.oldFormatters
        base.OnActionExecuted actionExecutedContext
        