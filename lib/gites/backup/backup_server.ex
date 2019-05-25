defmodule Gites.BackupServer do
  use GenServer
  alias Gites.BookingSystem
  alias Gites.BookingSystem.Availability

  @name :backup_server

  def start_link(_init_state) do
    bucket = System.get_env("S3_BUCKET")
    availabilities = BookingSystem.list_availabilities()
    bookings = BookingSystem.list_bookings()
    current_time = DateTime.utc_now()

    initial_state = %{
      availabilities: availabilities,
      bookings: bookings,
      current_time: current_time
    }

    ExAws.S3.put_object(
      bucket,
      "Backups/" <> DateTime.to_string(current_time),
      :erlang.term_to_binary(initial_state)
    )
    |> ExAws.request!()

    IO.inspect(initial_state)

    IO.puts("Starting the Backup server...")
    GenServer.start_link(__MODULE__, initial_state, name: @name)
  end
end
