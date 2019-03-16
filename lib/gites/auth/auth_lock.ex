defmodule Gites.AuthLock do 
  use Agent 

  def start_link(init_val) do 
    Agent.start_link(fn -> init_val end, name: __MODULE__)
  end 
  
  def is_locked do 
  	Agent.get(__MODULE__, & &1)
  end 

  def toogle_lock do 
  	Agent.update(__MODULE__, & !&1)
  end 

end 
