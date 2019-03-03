defmodule GitesWeb.UserController do
  use GitesWeb, :controller

  # Check user authenticated, otherwise halt
  plug Guardian.Plug.EnsureAuthenticated when action in [:index]

  alias Gites.Auth 

  def index(conn, _params) do
  	users = Auth.list_users()

    render conn, "users.json", users: users
  end

  def create(conn, %{"new_user" => user_params}) do
  	case Auth.create_user(user_params) do 
  		{:ok, _user} -> 
  			render conn, "success.json", %{}
  		{:error, %Ecto.Changeset{} = changeset} ->
  			render conn, "error.json", changeset
  	end 
  end  

end