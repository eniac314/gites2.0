defmodule GitesWeb.UserSessionController do 
	use GitesWeb, :controller 
  alias Gites.Repo
  alias Gites.Auth.User

  plug Guardian.Plug.EnsureAuthenticated when action in [:refresh]
  # require IEx

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
          |> render("login_success.json", username: userinfo["username"], jwt: jwt)
   
      {:error, reason, conn} ->
        conn
          |> put_status(500)
          |> render("login_error.json", reason: reason)
    end
  end

  def refresh(conn, _params) do
    jwt = Gites.Guardian.Plug.current_token(conn)
    user = Guardian.Plug.current_resource(conn)
    
    case Gites.Guardian.refresh(jwt) do
      {:ok, _old_stuff, {new_token, %{"exp" => _exp} = _new_claims}} -> 
        # IEx.pry
        render(conn, "login_success.json", username: user.username, jwt: new_token)
      {:error, reason} -> 
        conn
          |> put_status(500)
          |> render("login_error.json", reason: reason)
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
  			Pbkdf2.no_user_verify()
  			{:error, :not_found, conn}
  	end
  end 
  			
end 
