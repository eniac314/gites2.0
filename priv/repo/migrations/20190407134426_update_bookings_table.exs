defmodule Gites.Repo.Migrations.UpdateBookingsTable do
  use Ecto.Migration

  def change do
  	alter table(:bookings) do
  		add :pets, :string, default: nil
  		remove :title
	end
  end
end
