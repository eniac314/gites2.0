defmodule GitesWeb.AdminController do
  use GitesWeb, :controller

  plug :put_layout, false

  def index(conn, _params) do
    render(conn, "admin.html")
  end
end
