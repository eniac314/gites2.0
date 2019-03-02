defmodule GitesWeb.PageController do
  use GitesWeb, :controller

  plug :put_layout, false

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
