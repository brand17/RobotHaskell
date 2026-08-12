# RobotHaskell: Rigid-Body Rotational Dynamics Simulation

A deterministic, strongly-typed physical simulation engine implemented in pure Haskell. This framework models the complex rotational kinematics and equations of motion for interconnected falling rods experiencing multi-body gravitational and joint constraints.

The mathematical formulation for deriving the system's differential equations of motion is structurally based on Lagrangian mechanics, mapping generalized coordinates and acceleration vectors across pivot intersections, as outlined in [Physics StackExchange (Connected Rods Kinematics)](https://stackexchange.com).

## 🛠️ Architecture & Project Structure

The project follows standard, production-ready Haskell development conventions and utilizes the Cabal build ecosystem for reproducible compilations:

*   **`/app`**: Main application entry point containing the core execution loops, numerical integration steps, and state evaluation logic.
*   **`Robot.cabal`**: Package definition file specifying compilation targets, language extensions, and library dependencies.
*   **`cabal.project`**: Configuration file managing local build context and environment-specific constraints.

## 🧠 Core Engineering & Mathematical Principles

*   **Deterministic Kinematics Integration:** Models the non-linear differential equations governing connected rotational systems. State vectors (angular positions $\theta$ and angular velocities $\omega$) are updated via pure, deterministic numerical integration steps.
*   **Pure State Transitions:** Leverages pure functions to transform the state of the physical environment over time index $t$, completely eliminating mutable global state and side effects.
*   **Compile-Time Verification:** Utilizes Haskell's robust static type system to enforce dimensional boundaries, ensuring physical constants and constraints are validated at compile time.
*   **Industrial Build Stack:** Structured natively around the Cabal toolchain, demonstrating scalable software architecture layout rather than simple mathematical scripts.

## 🚀 Building and Running

Ensure you have GHC (Glasgow Haskell Compiler) and Cabal installed via `ghcup`.

1. Clone the repository:
   ```bash
   git clone https://github.com
   cd RobotHaskell
   ```

2. Build the project targets:
   ```bash
   cabal update
   cabal build
   ```

3. Execute the simulation loop:
   ```bash
   cabal run
   ```

## ⚖️ License

This project is open-source software licensed under the **BSD-3-Clause License**.
