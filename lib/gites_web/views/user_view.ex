defmodule GitesWeb.UserView do
  use GitesWeb, :view
  alias Ecto.Changeset

  def render("users.json", %{users: users}) do 
  	%{users: Enum.map(users, &user_json/1)}
  end 

  def render("success.json", _) do 
  	%{message: "success"}
  end 

  def render("error.json", changeset) do 
  	%{serverError: Changeset.traverse_errors(changeset, &translate_error/1)}
  end

  def user_json(user) do 
  	%{ username: user.username,
       email: user.email
     }
  end 

end