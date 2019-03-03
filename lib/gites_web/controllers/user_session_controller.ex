defmodule GitesWeb.UserSessionController do 
	use GitesWeb, :controller 
  alias Gites.Repo
  alias Gites.Auth.User

  plug Guardian.Plug.EnsureAuthenticated when action in [:delete]


  def create(conn, %{"login" => userinfo}) do 
  	case sign_in(conn, userinfo["username"], userinfo["password"]) do 
   		{:ok, conn} -> 
   			new_conn = Gites.Guardian.Plug.sign_in(conn, conn.assigns[:current_user])
        jwt = Gites.Guardian.Plug.current_token(new_conn)
        claims = Gites.Guardian.Plug.current_claims(new_conn)
        exp = Map.get(claims, "exp")
        
        new_conn
          |> put_resp_header("authorization", "Bearer #{jwt}")
          |> put_resp_header("x-expires", "#{exp}")
          |> render("login_success.json", jwt: jwt)
   
      {:error, reason, conn} ->
        conn
          |> put_status(500)
          |> render("login_error.json", reason: reason)
    end
  end

  def delete(conn, _params) do
    jwt = Gites.Guardian.Plug.current_token(conn)
    case Gites.Guardian.revoke(jwt) do 
      {:ok, _old_claims} -> 
        render(conn, "logged_out.json", %{})
      {:error, reasons} -> 
        render(conn, "logout_error.json", reasons)
    end
  end


  defp sign_in(conn, username, password) do 
  	user = Repo.get_by(User, username: username)

  	cond do
  		user && Pbkdf2.verify_pass(password, user.password_hash) ->
  			{:ok, assign(conn, :current_user, user)}
  		user -> 
  			{:error, :unauthorized, conn}
  		true -> 
  			Comeonin.Pbkdf2.dummy_checkpw()
  			{:error, :not_found, conn}
  	end
  end 
  			
end 
