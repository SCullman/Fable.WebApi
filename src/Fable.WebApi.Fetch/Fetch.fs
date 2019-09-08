module Fable.WebApi

open Fable.Core
open Fable.Core.JsInterop
open Djehuti.Json
open Fetch

type Fetch =
    /// **Description**
    ///
    /// Retrieves data from the specified resource.
    ///
    /// A decoder will be generated for the `'Response` type.
    /// 
    /// This method set the `ContentType` header to `"application/json"` if `data` is provided 
    ///
    /// An exception will be thrown if the decoder failed.
    ///
    /// **Parameters**
    ///   * `url` - parameter of type `string` - URL to request
    ///   * `httpMethod` - parameter of type `HttpMethod option` - Parameters passed to fetch - None will cause a **GET** Request
    ///   * `data` - parameter of type `Data option` - Data sent via the body, it will be converted to JSON before
    ///   * `properties` - parameter of type `RequestProperties list option` - Parameters passed to fetch
    ///   * `headers` - parameter of type `HttpRequestHeaders list option` - Parameters passed to fetch
    ///   * `isCamelCase` - parameter of type `bool option` - Options passed to Thoth.Json to control JSON keys representation
    ///   * `responseResolver` - parameter of type `ITypeResolver<'Response> option` - Used by Fable to provide generic type info
    ///
    /// **Output Type**
    ///   * `JS.Promise<Result<'Response,string>>`
    ///
    /// **Exceptions**
    ///
    static member tryFetchAs<'Data,'Response>
                  ( url : string,
                    ?httpMethod : HttpMethod,
                    ?data : 'Data,
                    ?properties : RequestProperties list,
                    ?headers: HttpRequestHeaders list,
                    ?isCamelCase : bool,
                    [<Inject>] ?responseResolver: ITypeResolver<'Response>) =

        let responseDecoder = Decode.Auto.generateDecoder<'Response>(?isCamelCase = isCamelCase, ?resolver = responseResolver)

        let headers = [ if data.IsSome then yield (ContentType "application/json") ] @ defaultArg headers []

        let properties =
            [ match httpMethod with
                | Some m -> yield Method m | _ -> ()
              if not (List.isEmpty headers) then yield requestHeaders headers
              match data with
                  | Some data -> 
                        yield (Encode.Auto.toString (4, data) |> (!^) |> Body)
                  | _ -> () ]
            @ defaultArg properties []

        promise {
             
            let! response = fetch url properties
            let! body = response.text()
            return Decode.fromString responseDecoder body
        }

    /// **Description**
    ///
    /// Retrieves data from the specified resource.
    ///
    /// A decoder will be generated for the `'Response` type.
    /// 
    /// This method set the `ContentType` header to `"application/json"` if `data` is provided 
    ///
    /// An exception will be thrown if the decoder failed.
    ///
    /// **Parameters**
    ///   * `url` - parameter of type `string` - URL to request
    ///   * `httpMethod` - parameter of type `HttpMethod option` - Parameters passed to fetch - None will cause a **GET** Request
    ///   * `data` - parameter of type `Data option` - Data sent via the body, it will be converted to JSON before
    ///   * `properties` - parameter of type `RequestProperties list option` - Parameters passed to fetch
    ///   * `headers` - parameter of type `HttpRequestHeaders list option` - Parameters passed to fetch
    ///   * `isCamelCase` - parameter of type `bool option` - Options passed to Thoth.Json to control JSON keys representation
    ///   * `responseResolver` - parameter of type `ITypeResolver<'Response> option` - Used by Fable to provide generic type info
    ///
    /// **Output Type**
    ///   * `JS.Promise<'Response>`
    ///
    /// **Exceptions**
    ///   * `System.Exception` - Contains information explaining why the decoder failed
    ///
    static member fetchAs<'Data,'Response>
            ( url : string,
              ?httpMethod : HttpMethod,
              ?data : 'Data,
              ?properties : RequestProperties list,
              ?headers: HttpRequestHeaders list,
              ?isCamelCase : bool,
              [<Inject>] ?responseResolver: ITypeResolver<'Response>) =

        promise {
            let! result = Fetch.tryFetchAs (url, ?httpMethod = httpMethod, ?data = data, ?properties = properties, ?headers = headers, ?isCamelCase = isCamelCase, ?responseResolver = responseResolver )
                          
            return match result with 
                        | Ok result -> result
                        | Error msg -> failwith msg }  
             
    static member get<'Response>
           (url : string,
            ?properties : RequestProperties list,
            ?headers: HttpRequestHeaders list,
            ?isCamelCase : bool,
            [<Inject>] ?responseResolver: ITypeResolver<'Response>)  =
        Fetch.fetchAs<_,'Response>(url, ?properties = properties, ?headers = headers, ?isCamelCase = isCamelCase, ?responseResolver = responseResolver)

    static member tryGet<'Response>
              (url : string,
               ?properties : RequestProperties list,
               ?headers: HttpRequestHeaders list,
               ?isCamelCase : bool,
               [<Inject>] ?responseResolver: ITypeResolver<'Response>) =
        Fetch.tryFetchAs<_,'Response>(url, ?properties = properties,?headers = headers, ?isCamelCase = isCamelCase, ?responseResolver = responseResolver)
    
    /// **Description**
    ///
    /// Send a **POST** request to the specified resource and apply the provided `decoder` to the response.
    ///
    /// This method set the `ContentType` header to `"application/json"`.
    ///
    /// A decoder will be generated for the `'Response` type.
    ///
    /// An exception will be thrown if the decoder failed.
    ///
    /// **Parameters** 
    ///   * `url` - parameter of type `string` - URL to request
    ///   * `data` - parameter of type `'Data` - Data sent via the body, it will be converted to JSON before
    ///   * `properties` - parameter of type `RequestProperties list option` - Parameters passed to fetch
    ///   * `headers` - parameter of type `HttpRequestHeaders list option` - Parameters passed to fetch
    ///   * `isCamelCase` - parameter of type `bool option` - Options passed to Thoth.Json to control JSON keys representation
    ///   * `responseResolver` - parameter of type `ITypeResolver<'Response> option` - Used by Fable to provide generic type info
    ///
    /// **Output Type**
    ///   * `JS.Promise<'Response>`
    ///
    /// **Exceptions**
    ///   * `System.Exception` - Contains information explaining why the decoder failed
    ///
    static member post<'Data,'Response> 
                   (url : string,
                    data : 'Data,
                    ?properties : RequestProperties list,
                    ?headers : HttpRequestHeaders list,
                    ?isCamelCase : bool,
                    [<Inject>] ?responseResolver: ITypeResolver<'Response>) =
        Fetch.fetchAs<'Data,'Response> (url,  ?data = Some data, ?httpMethod =Some HttpMethod.POST,?headers = headers, ?properties = properties, ?isCamelCase= isCamelCase, ?responseResolver=responseResolver)

    /// **Description**
    ///
    /// Send a **POST** request to the specified resource and apply the provided `decoder` to the response.
    ///
    /// This method set the `ContentType` header to `"application/json"`.
    ///
    /// An encoder will be generated for the `'Data` type.
    ///
    /// A decoder will be generated for the `'Response` type.
    ///
    /// If the decoder succeed, we return `Ok 'Response`.
    ///
    /// If the decoder failed, we return `Error "explanation..."`
    ///
    /// **Parameters**
    ///   * `url` - parameter of type `string` - URL to request
    ///   * `data` - parameter of type `'Data` - Data sent via the body, it will be converted to JSON before
    ///   * `properties` - parameter of type `RequestProperties list option` - Parameters passed to fetch
    ///   * `headers` - parameter of type `HttpRequestHeaders list option` - Parameters passed to fetch
    ///   * `isCamelCase` - parameter of type `bool option` - Options passed to Thoth.Json to control JSON keys representation
    ///   * `responseResolver` - parameter of type `ITypeResolver<'Response> option` - Used by Fable to provide generic type info
    ///
    /// **Output Type**
    ///   * `JS.Promise<Result<'Response,string>>`
    ///
    /// **Exceptions**
    ///
    static member tryPost<'Data,'Response>
                      ( url : string,
                        data : 'Data,
                        ?properties : RequestProperties list,
                        ?headers : HttpRequestHeaders list,
                        ?isCamelCase : bool,
                        [<Inject>] ?responseResolver: ITypeResolver<'Response>) =
        Fetch.tryFetchAs<'Data,'Response> (url, ?data = Some data, ?httpMethod =Some HttpMethod.POST,?headers = headers, ?properties = properties, ?isCamelCase= isCamelCase, ?responseResolver=responseResolver)
 
    /// **Description**
    ///
    /// Send a **PATCH** request to the specified resource and apply the provided `decoder` to the response.
    ///
    /// This method set the `ContentType` header to `"application/json"`.
    ///
    /// A decoder will be generated for the `'Response` type.
    ///
    /// An exception will be thrown if the decoder failed.
    ///
    /// **Parameters** 
    ///   * `url` - parameter of type `string` - URL to request
    ///   * `data` - parameter of type `'Data` - Data sent via the body, it will be converted to JSON before
    ///   * `properties` - parameter of type `RequestProperties list option` - Parameters passed to fetch
    ///   * `headers` - parameter of type `HttpRequestHeaders list option` - Parameters passed to fetch
    ///   * `isCamelCase` - parameter of type `bool option` - Options passed to Thoth.Json to control JSON keys representation
    ///   * `responseResolver` - parameter of type `ITypeResolver<'Response> option` - Used by Fable to provide generic type info
    ///
    /// **Output Type**
    ///   * `JS.Promise<'Response>`
    ///
    /// **Exceptions**
    ///   * `System.Exception` - Contains information explaining why the decoder failed
    ///
    static member patch<'Data,'Response> 
                   (url : string,
                    data : 'Data,
                    ?properties : RequestProperties list,
                    ?headers : HttpRequestHeaders list,
                    ?isCamelCase : bool,
                    [<Inject>] ?responseResolver: ITypeResolver<'Response>) =
        Fetch.fetchAs<'Data,'Response> (url,  ?data = Some data, ?httpMethod =Some HttpMethod.PATCH,?headers = headers, ?properties = properties, ?isCamelCase= isCamelCase, ?responseResolver=responseResolver)

    /// **Description**
    ///
    /// Send a **PATCH** request to the specified resource and apply the provided `decoder` to the response.
    ///
    /// This method set the `ContentType` header to `"application/json"`.
    ///
    /// An encoder will be generated for the `'Data` type.
    ///
    /// A decoder will be generated for the `'Response` type.
    ///
    /// If the decoder succeed, we return `Ok 'Response`.
    ///
    /// If the decoder failed, we return `Error "explanation..."`
    ///
    /// **Parameters**
    ///   * `url` - parameter of type `string` - URL to request
    ///   * `data` - parameter of type `'Data` - Data sent via the body, it will be converted to JSON before
    ///   * `properties` - parameter of type `RequestProperties list option` - Parameters passed to fetch
    ///   * `headers` - parameter of type `HttpRequestHeaders list option` - Parameters passed to fetch
    ///   * `isCamelCase` - parameter of type `bool option` - Options passed to Thoth.Json to control JSON keys representation
    ///   * `responseResolver` - parameter of type `ITypeResolver<'Response> option` - Used by Fable to provide generic type info
    ///
    /// **Output Type**
    ///   * `JS.Promise<Result<'Response,string>>`
    ///
    /// **Exceptions**
    ///
    static member tryPatch<'Data,'Response>
                      ( url : string,
                        data : 'Data,
                        ?properties : RequestProperties list,
                        ?headers : HttpRequestHeaders list,
                        ?isCamelCase : bool,
                        [<Inject>] ?responseResolver: ITypeResolver<'Response>) =
        Fetch.tryFetchAs<'Data,'Response> (url, ?data = Some data, ?httpMethod =Some HttpMethod.PATCH,?headers = headers, ?properties = properties, ?isCamelCase= isCamelCase, ?responseResolver=responseResolver)

    
    /// **Description**
    ///
    /// Send a **PUT** request to the specified resource and apply the provided `decoder` to the response.
    ///
    /// This method set the `ContentType` header to `"application/json"`.
    ///
    /// A decoder will be generated for the `'Response` type.
    ///
    /// An exception will be thrown if the decoder failed.
    ///
    /// **Parameters** 
    ///   * `url` - parameter of type `string` - URL to request
    ///   * `data` - parameter of type `'Data` - Data sent via the body, it will be converted to JSON before
    ///   * `properties` - parameter of type `RequestProperties list option` - Parameters passed to fetch
    ///   * `headers` - parameter of type `HttpRequestHeaders list option` - Parameters passed to fetch
    ///   * `isCamelCase` - parameter of type `bool option` - Options passed to Thoth.Json to control JSON keys representation
    ///   * `responseResolver` - parameter of type `ITypeResolver<'Response> option` - Used by Fable to provide generic type info
    ///
    /// **Output Type**
    ///   * `JS.Promise<'Response>`
    ///
    /// **Exceptions**
    ///   * `System.Exception` - Contains information explaining why the decoder failed
    ///
    static member put<'Data,'Response> 
                   (url : string,
                    data : 'Data,
                    ?properties : RequestProperties list,
                    ?headers : HttpRequestHeaders list,
                    ?isCamelCase : bool,
                    [<Inject>] ?responseResolver: ITypeResolver<'Response>) =
        Fetch.fetchAs<'Data,'Response> (url,  ?data = Some data, ?httpMethod =Some HttpMethod.PUT,?headers = headers, ?properties = properties, ?isCamelCase= isCamelCase, ?responseResolver=responseResolver)

    /// **Description**
    ///
    /// Send a **PUT** request to the specified resource and apply the provided `decoder` to the response.
    ///
    /// This method set the `ContentType` header to `"application/json"`.
    ///
    /// An encoder will be generated for the `'Data` type.
    ///
    /// A decoder will be generated for the `'Response` type.
    ///
    /// If the decoder succeed, we return `Ok 'Response`.
    ///
    /// If the decoder failed, we return `Error "explanation..."`
    ///
    /// **Parameters**
    ///   * `url` - parameter of type `string` - URL to request
    ///   * `data` - parameter of type `'Data` - Data sent via the body, it will be converted to JSON before
    ///   * `properties` - parameter of type `RequestProperties list option` - Parameters passed to fetch
    ///   * `headers` - parameter of type `HttpRequestHeaders list option` - Parameters passed to fetch
    ///   * `isCamelCase` - parameter of type `bool option` - Options passed to Thoth.Json to control JSON keys representation
    ///   * `responseResolver` - parameter of type `ITypeResolver<'Response> option` - Used by Fable to provide generic type info
    ///
    /// **Output Type**
    ///   * `JS.Promise<Result<'Response,string>>`
    ///
    /// **Exceptions**
    ///
    static member tryPutt<'Data,'Response>
                      ( url : string,
                        data : 'Data,
                        ?properties : RequestProperties list,
                        ?headers : HttpRequestHeaders list,
                        ?isCamelCase : bool,
                        [<Inject>] ?responseResolver: ITypeResolver<'Response>) =
        Fetch.tryFetchAs<'Data,'Response> (url, ?data = Some data, ?httpMethod =Some HttpMethod.PUT,?headers = headers, ?properties = properties, ?isCamelCase= isCamelCase, ?responseResolver=responseResolver)

    
    /// **Description**
    ///
    /// Send a **DELETE** request to the specified resource and apply the provided `decoder` to the response.
    ///
    /// This method set the `ContentType` header to `"application/json"`.
    ///
    /// A decoder will be generated for the `'Response` type.
    ///
    /// An exception will be thrown if the decoder failed.
    ///
    /// **Parameters** 
    ///   * `url` - parameter of type `string` - URL to request
    ///   * `data` - parameter of type `'Data` - Data sent via the body, it will be converted to JSON before
    ///   * `properties` - parameter of type `RequestProperties list option` - Parameters passed to fetch
    ///   * `headers` - parameter of type `HttpRequestHeaders list option` - Parameters passed to fetch
    ///   * `isCamelCase` - parameter of type `bool option` - Options passed to Thoth.Json to control JSON keys representation
    ///   * `responseResolver` - parameter of type `ITypeResolver<'Response> option` - Used by Fable to provide generic type info
    ///
    /// **Output Type**
    ///   * `JS.Promise<'Response>`
    ///
    /// **Exceptions**
    ///   * `System.Exception` - Contains information explaining why the decoder failed
    ///
    static member delete<'Data,'Response> 
                   (url : string,
                    data : 'Data,
                    ?properties : RequestProperties list,
                    ?headers : HttpRequestHeaders list,
                    ?isCamelCase : bool,
                    [<Inject>] ?responseResolver: ITypeResolver<'Response>) =
        Fetch.fetchAs<'Data,'Response> (url,  ?data = Some data, ?httpMethod =Some HttpMethod.DELETE,?headers = headers, ?properties = properties, ?isCamelCase= isCamelCase, ?responseResolver=responseResolver)

    /// **Description**
    ///
    /// Send a **DELETE** request to the specified resource and apply the provided `decoder` to the response.
    ///
    /// This method set the `ContentType` header to `"application/json"`.
    ///
    /// An encoder will be generated for the `'Data` type.
    ///
    /// A decoder will be generated for the `'Response` type.
    ///
    /// If the decoder succeed, we return `Ok 'Response`.
    ///
    /// If the decoder failed, we return `Error "explanation..."`
    ///
    /// **Parameters**
    ///   * `url` - parameter of type `string` - URL to request
    ///   * `data` - parameter of type `'Data` - Data sent via the body, it will be converted to JSON before
    ///   * `properties` - parameter of type `RequestProperties list option` - Parameters passed to fetch
    ///   * `headers` - parameter of type `HttpRequestHeaders list option` - Parameters passed to fetch
    ///   * `isCamelCase` - parameter of type `bool option` - Options passed to Thoth.Json to control JSON keys representation
    ///   * `responseResolver` - parameter of type `ITypeResolver<'Response> option` - Used by Fable to provide generic type info
    ///
    /// **Output Type**
    ///   * `JS.Promise<Result<'Response,string>>`
    ///
    /// **Exceptions**
    ///
    static member tryDelete<'Data,'Response>
                      ( url : string,
                        data : 'Data,
                        ?properties : RequestProperties list,
                        ?headers : HttpRequestHeaders list,
                        ?isCamelCase : bool,
                        [<Inject>] ?responseResolver: ITypeResolver<'Response>) =
        Fetch.tryFetchAs<'Data,'Response> (url, ?data = Some data, ?httpMethod =Some HttpMethod.DELETE,?headers = headers, ?properties = properties, ?isCamelCase= isCamelCase, ?responseResolver=responseResolver)

 