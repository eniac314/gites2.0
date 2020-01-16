defmodule Gites.Repo.Migrations.CreatePagesdata do
  use Ecto.Migration

  def change do
    create table(:pagesdata) do
      add :name, :string
      add :content, :text

      timestamps()
    end

    create unique_index(:pagesdata, [:name])
  end
end
