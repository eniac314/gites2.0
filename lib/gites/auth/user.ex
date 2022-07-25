defmodule Gites.Auth.User do
  use Ecto.Schema
  import Ecto.Changeset
  alias Gites.Auth.User

  schema "users" do
    field(:email, :string)
    field(:username, :string)
    field(:password, :string, virtual: true)
    field(:password_hash, :string)
    field(:is_active, :boolean, default: false)

    timestamps()
  end

  def changeset(%User{} = user, attrs) do
    user
    |> cast(attrs, [:email, :username, :password])
    |> validate_required([:email, :username, :password])
    |> unique_constraint(:email)
    |> unique_constraint(:username)
    |> validate_length(:password, min: 6, max: 100)
    |> validate_length(:username, min: 2, max: 100)
    |> put_pass_hash()
  end

  defp put_pass_hash(changeset) do
    case changeset do
      %Ecto.Changeset{valid?: true, changes: %{password: pass}} ->
        put_change(changeset, :password_hash, Pbkdf2.hash_pwd_salt(pass))

      _ ->
        changeset
    end
  end
end
