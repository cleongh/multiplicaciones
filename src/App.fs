module App

open Elmish
open Elmish.React
open Feliz

type Anterior = Nuevo | Bien | Mal
type State = { multiplicando : int; multiplicador : int; respuesta: int; anterior : Anterior }

let r = System.Random()

let otro () = 
  r.Next(1, 11)

type Msg =
  | Resolver
  | SetRespuesta of respuesta : int

let dameEstado a  r = { multiplicando = otro(); multiplicador = otro(); anterior = a; respuesta = r }

let init() = dameEstado Nuevo 0, Cmd.none

let calcula state =
  printfn $"{state.respuesta}"
  if (state.multiplicador * state.multiplicando = state.respuesta) 
  then Bien
  else Mal

let update msg state =
    match msg with
    | SetRespuesta r -> 
      printfn $"seteo {r}"
      { state with respuesta = r }, Cmd.none
    | Resolver -> dameEstado (calcula state) state.respuesta, Cmd.none

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    match state.anterior with
    | Nuevo -> Html.p "¡A multiplicar!"
    | Bien -> Html.p "¡Has acertado! 😄"
    | Mal -> Html.p ("¡Has fallado! 😓") // + string state.multiplicador + " x " + string state.multiplicando + " es " + (string (state.multiplicando * state.multiplicador)))
    Html.p ("¿Cuánto es " + string state.multiplicando + " por " + string state.multiplicador + "?")
    Html.p [
      Html.input [prop.onChange (fun (r:string) -> 
        r |> int |> SetRespuesta |>  dispatch)]
    ]
    Html.button [
      prop.onClick (fun _ -> dispatch Resolver)
      prop.text "¡Multiplicar!"
    ]

  ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run