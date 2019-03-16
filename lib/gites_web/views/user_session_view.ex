defmodule GitesWeb.UserSessionView do
  use GitesWeb, :view
  
  def render("login_success.json", %{username: username, jwt: jwt}) do 
  	%{username: username, jwt: jwt}
  end 

  def render("logout_success.json", _params) do 
  	%{message: "logged out"}
  end 

  def render("login_error.json", %{reason: reason}) do 
  	%{server_error: reason}
  end 

 
end 


