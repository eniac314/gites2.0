defmodule Gites.Repo.Migrations.AddMissingBookingsRef do
  use Ecto.Migration

  def change do
  	alter table(:availabilities) do 
      remove :bookingId
      add :bookingId, references(:bookings, on_delete: :delete_all)
    end
  end
end
