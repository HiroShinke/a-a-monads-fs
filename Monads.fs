

module Monads

// Identity 

type IdentityType<'a> = IdentityType of 'a

module Identity =
    let bind func (IdentityType x) = func x
    let result x = IdentityType x

type IdentityBuilder() =
    member x.Bind(expr,func) = Identity.bind func expr
    member x.Return(value) = Identity.result value

let ident = new IdentityBuilder()

let ret = ident {
       let func n = IdentityType (2 * n)
       let! x = func 10
       let! y = func 20
       return x + y
    }

printfn "ret=%A" ret

// Maybe

type MaybeType<'a> = Just of 'a | Nothing

module Maybe =
    let bind func x =
        match x with
            | Just v -> func v
            | Nothing -> Nothing
    let result x = Just x


type MaybeBuilder() =
    member x.Bind(expr, func) = Maybe.bind func expr
    member x.Return(value) = Maybe.result value

let foo1 x  = Just(x)
let foo2 x  =
    if x % 2 = 0 then
        Just (2 * x)
    else
        Nothing

let maybe = new MaybeBuilder()

let foo x = maybe {

    let! a = foo1(x)
    let! b = foo2(a)

    return a + b

    }

printfn "%A" (foo 10)
printfn "%A" (foo 15)

// Error

type ErrorType = Err of string

type EitherType<'e,'a> =
    | Left of 'e
    | Right of 'a

module Either =
    let bind func x =
        match x with
            | Right v -> func v
            | Left e  -> Left e
    let result x = Right x

type EitherBuilder () =
    member x.Bind(expr, func) = Either.bind func expr
    member x.Return(value)    = Either.result value

let either = EitherBuilder()

let throwError e = Left e
let catchError result handler =
    match result with
        | Right a -> Right a
        | Left e  -> handler e

let goo1 x  = Right x
let goo2 x  =
    if x % 2 = 0 then
        Right (2 * x)
    else
        throwError (Err "error even number")
let goo3 x  = Right (x + 10)


let goo x = either {

    let! a = goo1(x)
    let! b = goo2(a)
    let! c = goo3(b)

    return c
    }

printfn "%A" <| catchError (goo 15) (fun e ->
                                     printfn "%A" e
                                     Right 0)
                                     
printfn "%A" <| catchError (goo 10) (fun e ->
                                     printfn "%A" e
                                     Right 0)

// List


type ListBuilder() =
    member this.Bind (ls,f) =
        let s = seq {
            for x in ls do
                yield! (f x)
        }
        List.ofSeq s

    member this.Return v = [ v ]

let myList = new ListBuilder()

let testList = myList {

    let! x = [1; 2; 3]
    let! y = [4; 5; 6]

    return (x,y)
    }


printfn "%A" testList
printfn "%A" <| testList.GetType().FullName

// IO

// State

type StatefulFunc<'s,'a> = StatefulFunc of ('s -> 'a * 's)

let runState (StatefulFunc sf) s = sf s

module State =
    let bind func sf = StatefulFunc (
        fun s ->
            let (v,s2) = runState sf s
            runState (func v) s2
        )
    let result v = StatefulFunc (fun s -> (v,s))


type StateBuilder() =
    member x.Bind(expr,func) = State.bind func expr
    member x.Return(value)   = State.result value

let put s' = StatefulFunc (fun s -> ((),s'))
let get    = StatefulFunc (fun s -> (s,s))

let state = new StateBuilder ()

let hoo =
    let buff = "ABCDEF"
    StatefulFunc (fun i -> (buff.[i], i+1) )

let compHoo = state {
    let! a = hoo
    let! b = hoo
    let! c = hoo
    return (string a) + (string b) + (string c)
    }

printfn "%A" (runState compHoo 0)


let compHoo2 = state {
    do! (put 2)
    let! a = hoo
    let! b = hoo
    let! c = hoo
    return (string a) + (string b) + (string c)
    }

printfn "%A" (runState compHoo2 0)

              
// Reader

type ReaderFunc<'e,'a> = ReaderFunc of ('e -> 'a)

let runReader (ReaderFunc r) e = r e

module Reader =
    let bind func (ReaderFunc f) =
        ReaderFunc (
            fun e ->
            runReader (func (f e)) e
            )
    let result v = ReaderFunc ( fun _ -> v )

type ReaderBuilder () =
    member this.Bind(expr,func) = Reader.bind func expr
    member this.Return(v) = Reader.result v

let ask = ReaderFunc (fun e -> e)
let local f c = ReaderFunc (fun e ->
                            runReader c (f e)
                            )

let reader = ReaderBuilder()

let woo = ReaderFunc (fun (x :string) -> x + x )

let readerBlock = reader {
    let! a = woo
    let! b = woo
    let! c = local (fun (e :string) -> e.ToUpper() ) woo

    return System.String.Join("|", [a; b; c])
   }

printfn "%A" (runReader readerBlock "abc" )

// Writer

type WriterType<'w,'a> = WriterType of 'a * 'w

module Writer =
    let bind func (WriterType (v,w)) =
        let (WriterType (v',w'))  = func v
        WriterType (v', w + w')
    let result v = WriterType (v,"")

type WriterBuilder () =
    member this.Bind(expr,func) = Writer.bind func expr
    member this.Return(v) = Writer.result v

let writer = WriterBuilder()

let voo n m =
    let r = n + m
    WriterType (r, sprintf "%d add to %d makes %d\n" n m r)

let writerBlock = writer {
    let! a = voo 1 2
    let! b = voo a 10
    let! c = voo b 15
    return c
}

printfn "%A" writerBlock

// Continuation

type ContType<'r,'a> = Cont of (('a -> 'r) -> 'r)

let runCont (Cont k) f = k f

module Cont =
    let bind func (Cont c) = Cont ( fun k ->
                                    c <| fun a -> runCont (func a) k 
                                  )
    let result v = Cont ( fun k -> k v )
        

type ContBuilder () =
    member this.Bind(expr,func) = Cont.bind func expr
    member this.Return(v) = Cont.result v
    member this.ReturnFrom(v) = v

let callCC f = Cont <|
               fun k -> runCont (f (fun a -> Cont (fun _ -> k a)) ) k

let cont = ContBuilder()

let zoo (m :int) = Cont (fun k -> k m) // essentially this is return.

let contBlock1 = cont {
    let! a = Cont ( fun k -> k 0 )
    let! b = Cont ( fun k -> k (a + 10) )
    let! c = Cont ( fun k -> k (b + 10) )
    return c
    }

printfn "%A" (runCont contBlock1 (fun n -> n))


let contBlock2 = cont {
    let! a = zoo 0
    let! b = zoo (a + 10)
    let! c = zoo (b + 10)
    return c
    }

printfn "%A" (runCont contBlock2 (fun n -> n))


let contBlock3 n = 
    callCC (fun k ->
        cont {
            let! a = zoo n 
            let! b = zoo (a + 10)
            if b % 10 = 0 then
               return! (k 0)
            else
               return b
        }
    )


printfn "%A" (runCont (contBlock3 10) (fun n -> n))

printfn "%A" (runCont (contBlock3 11) (fun n -> n))

