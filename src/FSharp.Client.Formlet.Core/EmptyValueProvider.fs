(* Copyright 1999-2005 The Apache Software Foundation or its licensors, as
 * applicable.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

namespace FSharp.Client.Formlet.Core

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Linq.Expressions
open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

/// EmptyValueProvider is used when an empty value is required.
///  EmptyValueProvider supports several built-in F# types but allows
///  registering empty value for custom types
module EmptyValueProvider =

    type ProvidedValue =
        | NoEmptyValue
        | UncheckedEmptyValue
        | EmptyValue          of obj
        | EmptyValueCreator   of (unit -> obj)

    type GenericProvidedValue = Type*Type->ProvidedValue

    let internal GetStaticMethodInfo (f : Expr<'T>) : MethodInfo =
        match f with
        | Call (_, mi, _) ->
            if mi.IsGenericMethod then mi.GetGenericMethodDefinition ()
            else mi
        | _ -> failwithf "GetStaticMethodInfo requires an Call expression but received: %A" f

    let internal EmptyOption<'T> () : 'T option                         = None
    let internal EmptyList<'T> () : 'T list                             = List.Empty
    let internal EmptyMap<'K,'U when 'K : comparison> () : Map<'K, 'U>  = Map.empty

    let internal EmptyOptionMethodInfo  : MethodInfo    = GetStaticMethodInfo (<@ EmptyOption<int> () @>)
    let internal EmptyListMethodInfo    : MethodInfo    = GetStaticMethodInfo (<@ EmptyList<int> () @>)
    let internal EmptyMapMethodInfo     : MethodInfo    = GetStaticMethodInfo (<@ EmptyMap<int,int> () @>)

    let internal ToKeyValuePairs (vs : seq<'K*'U>) = vs |> Seq.map (fun (k,v) -> KeyValuePair (k,v))

    let internal ToDictionary (vs : seq<'K*'U>) =
        let dic = Dictionary<'K,'U> ()
        for k,v in vs do
            dic.[k] <- v
        dic

    let internal ToConcurrentDictionary (vs : seq<'K*'U>) : ConcurrentDictionary<'K,'U> =
        ConcurrentDictionary<'K, 'U> (vs |> ToKeyValuePairs)

    let internal Lookup (dv : 'U) (k : 'K) (dic : IDictionary<'K, 'U>) : 'U=
        let mutable v = Unchecked.defaultof<'U>
        if dic.TryGetValue (k, &v) then
            v
        else
            dv

    let internal ValueProviders : ConcurrentDictionary<Type, ProvidedValue> =
        [|
            typeof<string>      , EmptyValue          ""
        |] |> ToConcurrentDictionary

    let internal NoGenericValueProvider (t : Type, gt : Type) : ProvidedValue =
        NoEmptyValue

    let internal CreateLambda body =
        let unitPar = Expression.Parameter (typeof<unit>, "unit")
        let expr    = Expression.Lambda<Func<unit, obj>> (Expression.Convert (body, typeof<obj>), unitPar)
        let func    = expr.Compile ()
        let dvc     = fun () -> func.Invoke ()
        EmptyValueCreator dvc

    let internal CtorValueProvider (ctor : ConstructorInfo) (args : Expression []) : ProvidedValue =
        CreateLambda (Expression.New (ctor, args))

    let internal ArrayValueProvider (et : Type) : ProvidedValue =
        EmptyValue <| upcast System.Array.CreateInstance(et, 0)

    let internal GenericOptionValueProvider (t : Type, gt : Type) : ProvidedValue =
        let args    = t.GetGenericArguments()
        let vt      = args.[0]
        let mi      = EmptyOptionMethodInfo.MakeGenericMethod (vt)
        EmptyValue <| mi.Invoke (null, [||])

    let internal GenericListValueProvider (t : Type, gt : Type) : ProvidedValue =
        let args    = t.GetGenericArguments()
        let vt      = args.[0]
        let mi      = EmptyListMethodInfo.MakeGenericMethod (vt)
        EmptyValue <| mi.Invoke (null, [||])

    let internal GenericMapValueProvider (t : Type, gt : Type) : ProvidedValue =
        let args    = t.GetGenericArguments()
        let kt      = args.[0]
        let vt      = args.[1]
        let mi      = EmptyMapMethodInfo.MakeGenericMethod (kt, vt)
        EmptyValue <| mi.Invoke (null, [||])

    let (|HasDefaultCtor|IsArrayType|IsGenericType|NoMatch|) (t : Type) =
        let defaultCtor = t.GetConstructor([||])
        let arrayType   = if t.IsArray then Some (t.GetElementType ()) else None
        let genericType = if t.IsGenericType then Some (t.GetGenericTypeDefinition ()) else None

        match defaultCtor, genericType, arrayType with
        | null  ,   None    , None      -> NoMatch
        | _     ,   None    , None      -> HasDefaultCtor defaultCtor
        | null  ,   None    , Some et   -> IsArrayType et
        | null  ,   Some gt , None      -> IsGenericType gt
        | _                             -> NoMatch

    let internal SetValueProviderForType (t : Type) (providedValue : ProvidedValue) : unit = ValueProviders.[t] <- providedValue

    let rec internal GenericTupleValueProvider (t : Type, gt : Type)   : ProvidedValue =
        let args    = t.GetGenericArguments()
        let ctor    = t.GetConstructors () |> Array.find (fun ctor -> ctor.GetParameters().Length = args.Length)
        let paras   = ctor.GetParameters ()
        let args =
            paras
            |> Array.map (fun p ->
                Expression.Call (null, GetEmptyValueMethodInfo.MakeGenericMethod (p.ParameterType))
                :> Expression)
        CtorValueProvider ctor args

    and internal GenericValueProviders : ConcurrentDictionary<Type, GenericProvidedValue> =
        [|
            typedefof<Option<_>>                , GenericOptionValueProvider
            typedefof<List<_>>                  , GenericListValueProvider
            typedefof<Map<_,_>>                 , GenericMapValueProvider
            typedefof<Tuple<_>>                 , GenericTupleValueProvider
            typedefof<Tuple<_,_>>               , GenericTupleValueProvider
            typedefof<Tuple<_,_,_>>             , GenericTupleValueProvider
            typedefof<Tuple<_,_,_,_>>           , GenericTupleValueProvider
            typedefof<Tuple<_,_,_,_,_>>         , GenericTupleValueProvider
            typedefof<Tuple<_,_,_,_,_,_>>       , GenericTupleValueProvider
            typedefof<Tuple<_,_,_,_,_,_,_>>     , GenericTupleValueProvider
            typedefof<Tuple<_,_,_,_,_,_,_,_>>   , GenericTupleValueProvider
        |] |> ToConcurrentDictionary

    and internal GetEmptyValueForType (t : Type) : ProvidedValue =
        if t.IsGenericTypeDefinition then
            failwithf "Can't get a default value for generic type: %A" t.FullName

        let mutable provider = NoEmptyValue
        if ValueProviders.TryGetValue (t, &provider) then
            provider
        else
            if t.IsValueType then UncheckedEmptyValue
            else
                let provider = CreateProviderForType t
                SetValueProviderForType t provider
                provider

    and internal CreateProviderForType (t : Type) : ProvidedValue =
        match t with
        | HasDefaultCtor defaultCtor ->
            CtorValueProvider defaultCtor [||]
        | IsArrayType arrayType ->
            ArrayValueProvider arrayType
        | IsGenericType genericType ->
            let genericValueProvider = GenericValueProviders |> Lookup NoGenericValueProvider genericType
            genericValueProvider (t, genericType)
        | NoMatch ->
            NoEmptyValue

    /// Gets an empty value of type 'T
    and GetEmptyValue<'T> () =
        let t = typeof<'T>

        match GetEmptyValueForType t with
        | NoEmptyValue            ->
            failwithf
                "No default value provider or default ctor found for type: %A. Register a default value provider by EmptyValueProvider.SetEmptyValueProvider"
                t.FullName
        | UncheckedEmptyValue     ->
            Unchecked.defaultof<'T>
        | EmptyValue dv           ->
            dv :?> 'T
        | EmptyValueCreator dvc   ->
            dvc () :?> 'T

    and internal GetEmptyValueMethodInfo : MethodInfo = (GetStaticMethodInfo (<@ GetEmptyValue<int> () @>))

    /// Sets a value provider for a type 'T
    let SetValueProvider<'T> (providedValue : ProvidedValue) = SetValueProviderForType typeof<'T> providedValue

    /// Gets an array of all known value providers
    let GetValueProviders () : (Type*ProvidedValue) [] =
        ValueProviders.ToArray () |> Array.map (fun kv -> kv.Key, kv.Value)

    /// Sets a generic value provider for a generic type 'T
    let SetGenericValueProvider (genericType : Type) (genericProvidedValue : GenericProvidedValue) =
        if not genericType.IsGenericTypeDefinition then
            failwithf "genericType %A must be a generic type definition" genericType.FullName

        GenericValueProviders.[genericType] <- genericProvidedValue

    /// Gets an array of all known generic value providers
    let GetGenericValueProviders () : (Type*GenericProvidedValue) [] =
        GenericValueProviders.ToArray () |> Array.map (fun kv -> kv.Key, kv.Value)


