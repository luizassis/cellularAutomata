SIR = Model {
        
    -- the arguments of model and how it is built
    dimension = 30, -- dimension of cellular space
    finalTime = 10, -- amount with the final time of the simulation

    -- some attributes of Model that have internal semantics 
    -- simulation's parameters
    v = 0.6, -- virulescence [1, 0.6, 0.3]
    epsolon = 0.4, -- e pertence [0, 1]
    
    -- a mandatory function to describe how an instance of Model is created
    init = function(self) 
        
        -- a spatial location with homogeneous internal content
        self.cell = Cell {
                
            -- an optional function that describes how to initialize the Cell used as an instance of a CellularSpace
            init = function(cell)
                
                -- premise of everyone (population) initially (in percentage) is susceptible inside a cell
                cell.population = {susceptible = 1, infected = 0, recovered = 0}

                -- the movement factor stands for the probability of an infected individual belonging to the neighbour cell to be moved to the main cell
                cell.m = Random():number(1)

            end,         
            
            -- the random variable that designates the formulation of each is given for a discretization of infected, susceptible and recovered cells. 
            -- Besides, some functions of Cell that have internal semantics
            infected = function(cell)
                
                -- initialize the quantity of infected individuals inside the cell
                local quantity = 0

                -- for each agent check whether it is infected or not. If yes increment quantity
                forEachAgent(cell, function(agent)
                        if agent.discPopulation == "discInfected" then
                            quantity = quantity + 1
                        end
                end)
                
                --log cell and quantity
                --print(cell.x.." "..cell.y.." "..quantity)
                
                -- return quantity
                return (quantity)
            end,
            
            susceptible = function(cell)

                -- initialize the quantity of susceptible individuals inside the cell
                local quantity = 0

                -- for each agent check whether it is infected or not. If yes increment quantity
                forEachAgent(cell, function(agent)
                        if agent.discPopulation == "discSusceptible" then
                            quantity = quantity + 1
                        end
                end)
                
                --log cell and quantity
                --print(cell.x.." "..cell.y.." "..quantity)
                
                -- return quantity
                return (quantity)
            end,
            
            recovered = function(cell)

                -- initialize the quantity of recovered individuals inside the cell
                local quantity = 0                

                -- for each agent check whether it is infected or not. If yes increment quantity
                forEachAgent(cell, function(agent)
                        if agent.discPopulation == "discRecovered" then
                            quantity = quantity + 1
                        end
                end)
                
                --log cell and quantity
                --print(cell.x.." "..cell.y.." "..quantity)

                -- return quantity
                return (quantity)
            end,

            -- an optional function to define the changes in each time step      
            execute = function(cell) 
                
                -- local transition function used to calculate population infected (only part)
                cell.population.infected = (1 - self.epsolon) * cell.past.population.infected + self.v * cell.past.population.susceptible *cell.past.population.infected
                
                -- local transition function is used to calculate population susceptible (only part)
                cell.population.susceptible = cell.past.population.susceptible - self.v * cell.past.population.susceptible * cell.past.population.infected
                
                -- local transition function is used to calculate population recovered
                cell.population.recovered = cell.past.population.recovered + self.epsolon * cell.past.population.infected

                -- in case amount of agents is other than zero
                if (#cell:getAgents() ~= 0) then
                    
                    -- initialize sum equal to zero
                    local sum = 0
                    
                    -- for each neighbor 
                    forEachNeighbor(cell, function(neighbor, weight) 
                        --  product of three factors [the connection factor, the movement factor and the virulence of the epidemic]
                        local mi = weight * neighbor.m * self.v 
                        
                        -- compute one specific factor
                        sum = sum + #neighbor:getAgents() / #cell:getAgents() * mi * neighbor.past.population.infected
                    end)
                    
                    -- the remaining part for infected and susceptible
                    cell.population.infected = cell.population.infected + cell.past.population.susceptible * sum
                    cell.population.susceptible = cell.population.susceptible  - cell.past.population.susceptible * sum    
                end                            
                
            end
            
        }
               
        -- a multivalued set of Cells
        self.cs = CellularSpace {
            
            -- number of columns
            xdim = self.dimension,

            -- a cell with the description of attributes and functions
            instance = self.cell, 

            -- get an overview of susceptible inside cellular space
			susceptible = function(cs)

                -- initialize sum equal to zero
				local quantity = 0

                -- for each cell
				forEachCell(cs, function(cell)
                    -- sum the amount of agents inside a specific cell
					quantity = quantity + cell:susceptible()
				end)
                
                -- return quantity
				return quantity
			end,

            -- get an overview of recovered inside cellular space
			recovered = function(cs)

                -- initialize sum equal to zero
				local quantity = 0

                -- for each cell
				forEachCell(cs, function(cell)
                    -- sum the amount of agents inside a specific cell
					quantity = quantity + cell:recovered()
				end)

                -- return quantity
				return quantity
			end,

            -- get an overview of infected inside cellular space
			infected = function(cs)
				
                -- initialize sum equal to zero
                local quantity = 0

                -- for each cell
				forEachCell(cs, function(cell)
                    -- sum the amount of agents inside a specific cell
					quantity = quantity + cell:infected()
				end)

                -- return quantity
				return quantity
			end,
        }
        
        self.agent = Agent {
            
            -- assign susceptible to each agent
            discPopulation = "discSusceptible",
            
            -- describe the behavior of the agent each time step it is executed
            execute = function(agent)
                if agent.discPopulation == "discSusceptible" then
                    -- define whether still susceptible, infected or recovered
                    agent.discPopulation = Random{discSusceptible = 1 - agent:getCell().population.infected - agent:getCell().population.recovered, discInfected = agent:getCell().population.infected, discRecovered = agent:getCell().population.recovered}:sample()
                elseif agent.discPopulation == "discInfected" then
                            -- after infected is still recovered
                            agent.discPopulation = "discRecovered"
                end
            end
            
        }
        
        self.society = Society {
            -- define an agent with the description of attributes and functions
            instance = self.agent,
            -- the square of cellular space dimension * 100 [agents]
            quantity = self.dimension*self.dimension*100 
        }
        
        --self.cs:createNeighborhood { strategy = "vonneumann"}
        
        self.cs:createNeighborhood { 
            -- a m (columns) by n (rows) neighborhood within the CellularSpace
            strategy = "mxn",
            
            -- a function where the first argument is the Cell itself and the other represent a possible neighbor
            filter = function(cell, candidate)
                return ((cell.x <= (candidate.x + 1)) and (cell.x >= (candidate.x - 1))) and ((cell.y <= (candidate.y + 1)) and (cell.y >= (candidate.y - 1))) -- moore
                --return (((((cell.x + 1) >= candidate.x) or ((cell.x - 1) <= candidate.x)) and (cell.y == candidate.y)) or ((cell.x  == candidate.x) and (((cell.y + 1) >= candidate.y) or ((cell.y - 1) <= candidate.y)))) -- vonneumman
            end,
            
            -- connection factor relies on the existing transport infrastructures between the cells considered
            weight = function()
                -- if there exist the [three|two|one|zero] ways of transport between the cells;
                return Random{1, 0.6, 0.3, 0}:sample() 
            end
        }
        
        -- initial conditions of the single cell with infected individuals
        self.cs:get(15, 15).population = {susceptible = 0.7, infected = 0.3, recovered = 0}
                
        -- definition of environment with society and cs
        self.env = Environment{self.society, self.cs}
         
         -- maximum of 100 agents per cell
        self.env :createPlacement{max = 100}
        
        -- create a map with the spatial distribution of a given cellularspace
        self.map = Map{
            target = self.cs,
            -- create for infected
            select = "infected",
            -- precision = 2, {0.00, 0.01, 0.02, ...}
            min = 0,
            max = 100,
            -- number of colors to be used for plotting
            slices = 10,
            -- a table with the colors for the attributes
            color = {"white", "black"}
       }
       
       -- update a line chart showing the variation of one or more attributes (y axis) with the latest values of its target
       -- the values comprises 
       self.chart = Chart {
            target = self.cs,
            select = {"infected", "recovered", "susceptible"},
            color = {"red", "blue", "green"}
        }
        
        -- a timer is an event-based scheduler that runs the simulation.
        self.timer = Timer{
            -- call self.[map|cs|society|chart]
            Event{action = self.map},
            Event{action = self.cs},
            Event{action = self.society},
            Event{action = self.chart},
            Event{action = function(ev) self.map:save("file"..ev:getTime()..".png") end}
        }
    
    end

}

-- run SIR Model directly - in our case 44 iterations
epid = SIR{finalTime = 44}:run()