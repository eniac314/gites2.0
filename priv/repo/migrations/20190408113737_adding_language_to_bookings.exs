defmodule Gites.Repo.Migrations.AddingLanguageToBookings do
  use Ecto.Migration

  def change do
  	alter table(:bookings) do
  		add :language, :string, default: "french"
	end
  end
end
