defmodule Gites.Repo.Migrations.AddCleaningFee do
  use Ecto.Migration

  def change do
  	alter table(:bookings) do 
      add :options, :text
    end 
  end
end
