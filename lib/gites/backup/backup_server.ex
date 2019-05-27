defmodule Gites.BackupServer do
  use GenServer
  alias Gites.BookingSystem
  alias Gites.PagesData

  @name :backup_server

  # client API
  def start_link(_init_state) do
    bucket = System.get_env("S3_BUCKET")
    availabilities = BookingSystem.list_availabilities()
    bookings = BookingSystem.list_bookings()
    pages_data = PagesData.list_pagesdata()
    current_time = DateTime.utc_now()

    current_data = %{
      availabilities: availabilities,
      bookings: bookings,
      pages_data: pages_data,
      current_time: current_time
    }

    backups_meta =
      ExAws.S3.list_objects(bucket, delimiter: "/", prefix: "Backups/")
      |> ExAws.request!()

    newest_backup_meta =
      case backups_meta.body.contents do
        [] ->
          nil

        _ ->
          backups_meta.body.contents
          |> Enum.max_by(fn c -> DateTime.from_iso8601(c.last_modified) end)
      end

    case newest_backup_meta do
      nil ->
        create_new_backup(current_data)

      backup_meta ->
        {:ok, lm, _} = DateTime.from_iso8601(backup_meta.last_modified)

        if DateTime.diff(current_time, lm) > 7 * 24 * 60 * 60 do
          create_new_backup(current_data)
        end
    end

    initial_state = %{current_backup: current_data}

    IO.puts("Starting the Backup server...")
    GenServer.start_link(__MODULE__, initial_state, name: @name)
  end

  def get_current_backup do
    GenServer.call(@name, :get_current_backup)
  end

  def import_backup do
    GenServer.cast(@name, :import_backup)
  end

  def export_backup do
    GenServer.call(@name, :export_backup)
  end

  def list_backups do
    GenServer.call(@name, :list_backups)
  end

  def load_backup(key) do
    GenServer.cast(@name, {:load_backup, key})
  end

  def delete_backup(key) do
    GenServer.cast(@name, {:delete_backup, key})
  end

  defp create_new_backup(backup) do
    bucket = System.get_env("S3_BUCKET")
    IO.puts("Creating backup...")

    ExAws.S3.put_object(
      bucket,
      "Backups/" <> DateTime.to_string(backup.current_time),
      :erlang.term_to_binary(backup)
    )
    |> ExAws.request!()
  end

  # server callbacks 
  def handle_call(:get_current_backup, _from, %{current_backup: backup} = state) do
    {:reply, backup, state}
  end

  def handle_cast(:import_backup, state) do
    availabilities = BookingSystem.list_availabilities()
    bookings = BookingSystem.list_bookings()
    pages_data = PagesData.list_pagesdata()
    current_time = DateTime.utc_now()

    current_data = %{
      availabilities: availabilities,
      bookings: bookings,
      pages_data: pages_data,
      current_time: current_time
    }

    create_new_backup(current_data)

    IO.puts("Deleting current data from database...")

    IO.puts("Deleting availabilities...")
    Enum.map(availabilities, fn a -> BookingSystem.delete_availability(a) end)

    IO.puts("Deleting bookings...")
    Enum.map(bookings, fn b -> BookingSystem.delete_booking(b) end)

    IO.puts("Deleting pages data...")
    Enum.map(pages_data, fn pd -> PagesData.delete_page_data(pd) end)

    IO.puts("Importing data...")

    IO.puts("Importing availabilities...")

    Enum.map(
      state.current_backup.availabilities,
      fn a ->
        Map.from_struct(a)
        |> BookingSystem.create_availability()
      end
    )

    IO.puts("Importing bookings...")

    Enum.map(
      state.current_backup.bookings,
      fn b ->
        Map.from_struct(b)
        |> BookingSystem.create_booking()
      end
    )

    IO.puts("Importing pages_data...")

    Enum.map(
      state.current_backup.pages_data,
      fn pd ->
        Map.from_struct(pd)
        |> PagesData.create_page_data()
      end
    )

    {:noreply, state}
  end

  def handle_call(:export_backup, _from, state) do
    {:ok, filename} = DateTime.to_iso8601(state.current_time)
    result = %{filename: filename, content: :erlang.term_to_binary(state)}
    {:reply, result, state}
  end

  def handle_call(:list_backups, _from, state) do
    bucket = System.get_env("S3_BUCKET")

    backups_meta =
      ExAws.S3.list_objects(bucket, delimiter: "/", prefix: "Backups/")
      |> ExAws.request!()

    {:reply, backups_meta.body.contents, state}
  end

  def handle_cast({:load_backup, key}, _state) do
    bucket = System.get_env("S3_BUCKET")

    response =
      ExAws.S3.get_object(bucket, key)
      |> ExAws.request!()

    new_state = %{current_backup: :erlang.binary_to_term(response.body)}

    {:noreply, new_state}
  end

  def handle_cast({:delete_backup, key}, state) do
    bucket = System.get_env("S3_BUCKET")

    ExAws.S3.delete_object(bucket, key)
    |> ExAws.request!()

    {:noreply, state}
  end
end
