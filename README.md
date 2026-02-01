# EU-Energy-trends-and-the-German-Market

## Introduction
Energy plays a fundamental role in a functional society. The process of a
civilization is even measured in energy consumption (Kardashev scale). The
climate crisis forces us to rethink our energy consumption and generation. The
green transition has become a top priority under the European Green Deal and the
REPowerEU plan, which aim to achieve climate neutrality by 2050 and reduce
dependence on imports. Furthermore the aggression war from Russia has shown
us how dangerous it can be for our industry to depend on foreign energy. The aim
of this project is to analyze the trends and patterns in Europe's energy
consumption and generation, with focus on the market stability, influence of
renewable energy on electricity prices, how the Russia’s invasion of Ukraine
caused gas prices to increase dramatically ,which also influenced the electricity
prices. We also build regression model where our independent variable was
electricity prices.

## Summary of relevant prior info
The European Green Deal (2019) is the growth strategy to make the EU clima
neutral. The Russian’s Invasion of Ukraine caused the 2021-2023 energy crisis
in which record gas and electricity prices placed pressure on companies that rely
on energy as a production input. The REPowerEU Plan (2022) was a direct
response to the energy crisis, focusing on energy independence and
acceleration of the renewable development. Europe imports liquefied natural gas
(LNG) and oil. Germany's early feed-in tariffs, introduced in 2000 with the
Renewable Energy Sources Act (EEG), guaranteed a fixed, long-term payment
for renewable energy fed into the grid

## Purpose
The purpose of this project is to analyze trends and relationships in Europe’s energy
system to understand how policy changes, market shocks, and renewable growth
interact.

## Preparation
We conducted thorough research to identify credible academic and official data
sources. This included reviewing relevant literature and assessing the expertise of the
authors. To ensure data reliability, we focused on official market platforms such as
SMARD and ENTSO-E, studying their documentation in detail. Once we identified
suitable and downloadable datasets, we considered how best to integrate them into our
narrative and analysis

The European energy dataset was compiled from ember-energy.com (EMBER), a
global energy think tank that aims to accelerate the clean energy transition with data
and policy. It contained the data for all the European countries, with category,
subcategory and variables of the energy type, the unit, in which the energy was
measured (TWh, % etc.) and the value of the measured energy.
The Germany energy datasets were compiled from SMARD.de, the official transparency
platform of the German electricity market, which provides detailed time-series data on
electricity generation, consumption, and cross-border physical flows. Multiple CSV files
over the years of 2015-2025 were downloaded to capture Germany’s actual generation,
installed generation capacity, forecasted and actual consumption, and cross-border
imports and exports with neighboring countries over time. Each file contained a
consistent temporal structure, allowing them to be cleaned and merged into a single
analytical dataset for each relevant category.
Data preparation involved aligning column formats, harmonizing date fields, and
ensuring numerical consistency across all categories. For generation, the datasets on
actual and installed capacity were merged chronologically and cleaned for empty or
inactive energy sources. For consumption, actual and forecasted electricity demand
files were combined and standardized to a daily resolution. For cross-border physical
flows, yearly datasets were merged to form a continuous multi-year series, retaining all
records to reflect the evolution of trade intensity with neighboring countries.
Data preparation in this project also involved manually writing the gas market data down
from the BNA since it was not downloadable.
