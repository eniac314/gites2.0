defmodule Gites.AuthLock do 
  use GenServer 

  @name :auth_lock_server
  @expiration :timer.minutes(2)
  @countdown_interval :timer.seconds(10)

  def start_link(_init_state) do 
  	IO.puts "Starting the AuthLock server..."
  	GenServer.start_link(__MODULE__, {:false,  @expiration}, name: @name)
  end 

  def is_locked do 
  	GenServer.call @name, :is_locked
  end 

  def lock do 
  	GenServer.cast @name, :lock
  end 

  def unlock do 
  	GenServer.cast @name, :unlock
  end

  def refresh_expiration do 
  	GenServer.cast @name, :refresh 
  end

  defp schedule_countdown do
    Process.send_after(self(), :countdown, @countdown_interval)    
  end 


  def handle_call(:is_locked, _from, {state,timer}) do 
  	{:reply, state, {state,timer}}
  end 

  def handle_cast(:lock,  {_state,_timer}) do 
  	IO.puts "AuthLock server: locked"
  	schedule_countdown()
  	{:noreply, {:true, @expiration}}
  end 

  def handle_cast(:unlock, {_state, _timer}) do 
  	IO.puts "AuthLock server: manual unlock"
  	{:noreply, {:false,  @expiration}}
  end

  def handle_cast(:refresh, {state, _timer}) do 
  	{:noreply, {state,  @expiration}}
  end 

  def handle_info(:countdown,{state, timer}) do 
    if timer <= 0 do 
    	IO.puts "AuthLock server: automatic unlock"
    	{:noreply, {false, @expiration}}
    else
      schedule_countdown()
      # IO.puts "AuthLock server: #{inspect({state, timer})}"
      {:noreply, {state, timer - @countdown_interval}}
    end 
  end 

end 
