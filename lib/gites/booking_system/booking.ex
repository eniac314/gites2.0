defmodule Gites.BookingSystem.Booking do
  use Ecto.Schema
  import Ecto.Changeset

  schema "bookings" do
    field(:add_address, :string)
    field(:address, :string)
    field(:check_in, :integer)
    field(:check_out, :integer)
    field(:city, :string)
    field(:comments, :string)
    field(:confirmed, :boolean, default: false)
    field(:country, :string)
    field(:email, :string)
    field(:first_name, :string)
    field(:last_name, :string)
    field(:nbr_adults, :integer)
    field(:nbr_children, :integer)
    field(:phone1, :string)
    field(:phone2, :string)
    field(:postcode, :integer)
    field(:pets, :string, default: nil)
    field(:options, :string, default: nil)
    field(:language, :string, default: "french")

    timestamps()
  end

  @doc false
  def changeset(booking, attrs) do
    booking
    |> cast(attrs, [
      :check_in,
      :check_out,
      :pets,
      :first_name,
      :last_name,
      :address,
      :add_address,
      :postcode,
      :city,
      :country,
      :phone1,
      :phone2,
      :email,
      :nbr_adults,
      :nbr_children,
      :comments,
      :options,
      :confirmed,
      :language,
      :id
    ])
    |> validate_required([
      :check_in,
      :check_out,
      :first_name,
      :last_name,
      :address,
      :postcode,
      :city,
      :country,
      :phone1,
      :email,
      :nbr_adults
    ])
  end
end
