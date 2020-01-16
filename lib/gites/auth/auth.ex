defmodule Gites.Auth do 

  import Ecto.Query, warn: false
  alias Gites.Repo

  alias Gites.Auth.User 

  def list_users do
    Repo.all(User)
  end

  def get_user!(id) do
     Repo.get!(User, id)
  end 

  def create_user(attrs \\ %{}) do
    %User{}
    |> User.changeset(attrs)
    |> Repo.insert()
  end

end 