defmodule Gites.PagesData.PageData do
  use Ecto.Schema
  import Ecto.Changeset


  schema "pagesdata" do
    field :content, :string
    field :name, :string

    timestamps()
  end

  @doc false
  def changeset(page_data, attrs) do
    page_data
    |> cast(attrs, [:name, :content])
    |> validate_required([:name, :content])
    |> unique_constraint(:name)
  end
end
