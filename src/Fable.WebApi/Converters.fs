
// Code taken from Thoth.Json.Net 2.5.0
// Djehuti is the egyptian spelling for Thoth
module Djehuti.Json.Net.Converters

open System
open System.Collections.Concurrent
open System.Reflection
open FSharp.Reflection
open Newtonsoft.Json

let private advance(reader: JsonReader) =
    reader.Read() |> ignore

/// ATTENTION: The reader should be located in the first element of the array (not in StartArray)
let private readElements(reader: JsonReader, itemTypes: Type[], serializer: JsonSerializer): obj seq =
    let mutable index = 0
    let elems = ResizeArray()
    while reader.TokenType <> JsonToken.EndArray do
        elems.Add(serializer.Deserialize(reader, itemTypes.[index]))
        index <- index + 1
        advance reader
    upcast elems

let private getUci (reader: JsonReader) t name =
    FSharpType.GetUnionCases(t)
    |> Array.tryFind (fun uci -> uci.Name = name)
    |> function
        | Some uci -> uci
        | None -> failwithf "Cannot find case %s in %s (path: %s)" name (string t) reader.Path

let private makeUnion (reader: JsonReader) uci values =
    try FSharpValue.MakeUnion(uci, values)
    with ex -> failwithf "Cannot create union %s (case %s) with %A (path: %s): %s"
                    (string uci.DeclaringType) uci.Name values reader.Path ex.Message

type OptionConverter() =
    inherit JsonConverter()
    override __.CanConvert(t) =
        t.Name = "FSharpOption`1" && t.Namespace = "Microsoft.FSharp.Core"
    override __.WriteJson(writer, value, serializer) =
        let t = value.GetType()
        let _, fields = FSharpValue.GetUnionFields(value, t)
        if fields.Length = 0
        then writer.WriteNull()
        else serializer.Serialize(writer, fields.[0])
    override __.ReadJson(reader, t, _existingValue, serializer) =
        match reader.TokenType with
        | JsonToken.Null -> makeUnion reader (getUci reader t "None") [||]
        | _ -> let value = serializer.Deserialize(reader, t.GenericTypeArguments.[0])
               makeUnion reader (getUci reader t "Some") [|value|]

type UnionConverter() =
    inherit JsonConverter()
    override __.CanConvert(t) =
        FSharpType.IsUnion t
        && t.Name <> "FSharpList`1"
        && t.Name <> "FSharpOption`1"
    override __.WriteJson(writer, value, serializer) =
        let t = value.GetType()
        let uci, fields = FSharpValue.GetUnionFields(value, t)
        if fields.Length = 0 then
            serializer.Serialize(writer, uci.Name)
        else
            let ar = Array.zeroCreate<obj> (fields.Length + 1)
            ar.[0] <- box uci.Name
            Array.Copy(fields, 0, ar, 1, fields.Length)
            serializer.Serialize(writer, ar)
    override __.ReadJson(reader, t, _existingValue, serializer) =
        match reader.TokenType with
        | JsonToken.String ->
            let name = serializer.Deserialize(reader, typeof<string>) :?> string
            makeUnion reader (getUci reader t name) [||]
        | JsonToken.StartArray ->
            advance reader
            let name =
                match reader.TokenType with
                | JsonToken.String -> reader.Value :?> string
                | token -> failwithf "Expecting string (case name) as first element of array but got %s (path: %s)"
                                        (Enum.GetName(typeof<JsonToken>, token)) reader.Path
            let uci = getUci reader t name
            advance reader
            let itemTypes = uci.GetFields() |> Array.map (fun pi -> pi.PropertyType)
            let values = readElements(reader, itemTypes, serializer) |> Seq.toArray
            makeUnion reader uci values
        | token -> failwithf "Expecting string or array for %s but got %s (path: %s)"
                        (string t) (Enum.GetName(typeof<JsonToken>, token)) reader.Path

type TupleConverter() =
    inherit JsonConverter()
    override __.CanConvert(t) =
        FSharpType.IsTuple t
    override __.WriteJson(writer, value, serializer) =
        let values = FSharpValue.GetTupleFields(value)
        serializer.Serialize(writer, values)
    override __.ReadJson(reader, t, _existingValue, serializer) =
        match reader.TokenType with
        | JsonToken.StartArray ->
            advance reader
            let values = readElements(reader, FSharpType.GetTupleElements(t), serializer) |> Seq.toArray
            try FSharpValue.MakeTuple(values, t)
            with ex -> failwithf "Cannot create tuple %s with %A (path: %s): %s"
                            (string t) values reader.Path ex.Message
        | JsonToken.Null -> null // {"tuple": null}
        | token -> failwithf "Expecting array for tuple got %s (path: %s)"
                        (Enum.GetName(typeof<JsonToken>, token)) reader.Path

type MapConverter() =
    inherit JsonConverter()
    override __.CanConvert(t) =
        t.Name = "FSharpMap`2"
        && t.Namespace = "Microsoft.FSharp.Collections"
    override __.WriteJson(writer, value, serializer) =
        let mutable kvProps: PropertyInfo[] = null
        writer.WriteStartArray()
        for kv in value :?> System.Collections.IEnumerable do
            if isNull kvProps then
                kvProps <- kv.GetType().GetProperties()
            writer.WriteStartArray()
            serializer.Serialize(writer, kvProps.[0].GetValue(kv))
            serializer.Serialize(writer, kvProps.[1].GetValue(kv))
            writer.WriteEndArray()
        writer.WriteEndArray()
    override __.ReadJson(reader, t, _existingValue, serializer) =
        match reader.TokenType with
        | JsonToken.StartArray ->
            let mapTypes = t.GetGenericArguments()
            let tupleType = typedefof<obj*obj>.MakeGenericType(mapTypes)
            let listType = typedefof< ResizeArray<obj> >.MakeGenericType([|tupleType|])
            let list = System.Activator.CreateInstance(listType)
            let meth = listType.GetMethod("Add")
            advance reader
            while reader.TokenType <> JsonToken.EndArray do
                advance reader
                let values = readElements(reader, mapTypes, serializer)
                let tuple = FSharpValue.MakeTuple(Seq.toArray values, tupleType)
                meth.Invoke(list, [|tuple|]) |> ignore
                advance reader
            let cons = t.GetConstructors() |> Array.head
            cons.Invoke([|list|])
        | token -> failwithf "Expecting array for map got %s (path: %s)"
                        (Enum.GetName(typeof<JsonToken>, token)) reader.Path

let converters =
    [|
        OptionConverter() :> JsonConverter
        UnionConverter() :> JsonConverter
        TupleConverter() :> JsonConverter
        MapConverter() :> JsonConverter
    |]

type CacheConverter(converters: JsonConverter[]) =
    inherit JsonConverter()
    let cache = ConcurrentDictionary<Type, JsonConverter>()
    static let mutable singleton: CacheConverter option = None
    static member Singleton =
        match singleton with
        | Some x -> x
        | None ->
            let x = CacheConverter(converters)
            singleton <- Some x
            x
    override __.CanConvert(t) =
        let conv =
            cache.GetOrAdd(t, fun t ->
                converters
                |> Array.tryFind (fun conv -> conv.CanConvert(t))
                |> Option.toObj)
        not(isNull conv)
    override __.WriteJson(writer, value, serializer) =
        match cache.TryGetValue(value.GetType()) with
        | false, _
        | true, null -> serializer.Serialize(writer, value)
        | true, conv -> conv.WriteJson(writer, value, serializer)
    override __.ReadJson(reader, t, existingValue, serializer) =
        match cache.TryGetValue(t) with
        | false, _
        | true, null -> serializer.Deserialize(reader, t)
        | true, conv -> conv.ReadJson(reader, t, existingValue, serializer)