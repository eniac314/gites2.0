defmodule Gites.Repo.Migrations.CreateAvailabilities do
  use Ecto.Migration

  def change do
    create table(:availabilities) do
      add :date, :integer
      add :availability, :string
      add :bookingId, references(:bookings, on_delete: :nothing)

      timestamps()
    end

    create unique_index(:availabilities, [:date])
    create index(:availabilities, [:bookingId])
  end
end
