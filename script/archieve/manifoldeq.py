from sympy import symbols, Eq, solve

# Define the variables
P, Qd, Qs = symbols('P Qd Qs')

# Define the demand and supply curves
# Qd = a - b*P (a and b are constants)
# Qs = c + d*P (c and d are constants)
a, b = 100, 2
c, d = -20, 3

demand_curve = Eq(Qd, a - b*P)
supply_curve = Eq(Qs, c + d*P)

# Equate the demand and supply curves to find the equilibrium point
equilibrium = Eq(Qd, Qs)

# Substitute the expressions for Qd and Qs from the demand and supply curves
equilibrium = equilibrium.subs({Qd: demand_curve.rhs, Qs: supply_curve.rhs})

# Solve for the equilibrium price P
equilibrium_price = solve(equilibrium, P)[0]

# Calculate the equilibrium quantity by substituting the equilibrium price back into either the demand or supply curve
equilibrium_quantity = demand_curve.rhs.subs(P, equilibrium_price)

print(f"Equilibrium price: {equilibrium_price}")
print(f"Equilibrium quantity: {equilibrium_quantity}")
