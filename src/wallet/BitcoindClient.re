type config = {
  bitcoindUrl: string,
  rpcUser: string,
  rpcPassword: string
};

let getBlockHeight = ({bitcoindUrl, rpcUser, rpcPassword}) => {
  let jsonRPC =
    Json.Encode.(
      object_([
        ("jsonrpc", string("1.0")),
        ("method", string("getblockcount"))
      ])
    )
    |> Json.stringify;
  let authString =
    Node_buffer.fromString({j|$(rpcUser):$(rpcPassword)|j})
    |> BufferExt.toStringWithEncoding("base64");
  Js.Promise.(
    Fetch.fetchWithInit(
      bitcoindUrl,
      Fetch.RequestInit.make(
        ~method_=Fetch.Post,
        ~headers=
          Fetch.HeadersInit.make({"Authorization": {j|Basic $(authString)|j}}),
        ~body=Fetch.BodyInit.make(jsonRPC),
        ()
      )
    )
    |> then_(Fetch.Response.json)
    |> then_(obj => resolve(Json.Decode.(field("result", int, obj))))
  );
  /* .then(resp => resp.json()) */
  /* .then(respObj => respObj.result) */
};
