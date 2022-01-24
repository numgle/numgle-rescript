open Express

let app = expressCjs()

app->use(jsonMiddleware())

app->get("/:value", (req, res) => {
  let params = req->params
  let input = params["value"]

  let _ = switch input->Js.Nullable.toOption {
    | Some(value) =>
      res->set("content-type", "text/plain")
      res->status(200)->send(value->NumgleConverter.convert)
    | None => res->status(400)->json({ "error": "Invalid input" })
  }
})

let port = 8086
let _ = app->listenWithCallback(port, _ => {
  Js.log("Server started at: http://localhost:" ++ port->Belt.Int.toString)
})