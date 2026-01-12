# intelligent_route_planner

This repository contains an academic project focused on the design and implementation of a **logical reasoning system for route planning** of a mobile robot operating in a discretized industrial environment. The system combines **declarative reasoning in Prolog** with **execution, integration, and visualization in Python**, allowing the study of constrained navigation and cost-based planning.

## Repository Structure

The repository contains two main files:

### `route_planner.pl`
This file implements the **logical reasoning model** using Prolog. It defines:

- The representation of the environment as a grid of cells.
- Physical constraints such as floor types and restricted zones.
- Access control mechanisms based on doors and keys.
- Dynamic elements such as barriers controlled by switches.
- Special non-local transitions using conveyor belts with configurable costs.
- A complete implementation of the **A\*** algorithm operating on an extended state space that includes position, collected keys, and active switches.

This file is responsible for all the decision-making and route planning logic.

### `route_planner.py`
This file acts as the **integration and execution layer**. It is responsible for:

- Initializing the environment (map, zones, doors, barriers, items, conveyors).
- Translating the environment description into Prolog facts using `janus_swi`.
- Triggering route planning queries from Python to Prolog.
- Receiving and interpreting the computed routes.
- Providing a graphical visualization of the environment, the robot, and the planned route using `matplotlib`.

Python does not reimplement any planning logic; it strictly orchestrates and visualizes the reasoning performed in Prolog.

## Requirements

To run the project, the following software is required:

- Python 3.9 or higher
- SWI-Prolog (tested with version 10.x)
- Python dependencies:
  - `janus_swi`
  - `numpy`
  - `matplotlib`

SWI-Prolog must be properly installed and available in the system `PATH` for `janus_swi` to work correctly.

## How to Run

The system is executed directly from the Python file.

```bash
python route_planner.py
