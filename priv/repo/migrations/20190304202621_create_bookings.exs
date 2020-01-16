defmodule Gites.Repo.Migrations.CreateBookings do
  use Ecto.Migration

  def change do
    create table(:bookings) do
      add :check_in, :integer
      add :check_out, :integer
      add :title, :string
      add :first_name, :string
      add :last_name, :string
      add :address, :string
      add :add_address, :string
      add :postcode, :integer
      add :city, :string
      add :country, :string
      add :phone1, :string
      add :phone2, :string
      add :email, :string
      add :nbr_adults, :integer
      add :nbr_children, :integer
      add :comments, :text
      add :confirmed, :boolean, default: false, null: false

      timestamps()
    end

  end
end
