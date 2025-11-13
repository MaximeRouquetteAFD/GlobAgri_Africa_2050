# About the model

GlobAgri is a model of biomass balance, in physical units, which at the world level ensures for each agricultural product considered the balance between its supply and its different uses. The model defines a balance such that domestic production plus imports equalize the sum of uses: human food, animal feed, exports, non-food processing and losses/waste.

**`Production + imports = Food + Feed + Seed + Extraction + Other non-food uses + Losses`**

The original model was built on 21 regions worldwide, including 3 large regions that together represented Africa and 18 other regions for the rest of the world. With AFD’s support, these three African regions were disaggregated into the 54 African countries, so the version of the model used in this study now consists of the 54 African countries plus the remaining 18 world regions. Futhermore, the modeling is carried out by 2050 starting from a base year of 2018 (before Covid which disrupted trade that was slow to recover).

# Principals model data sources

**FAOstat :** source used for the 33 aggregates of agricultural products

|  |  |
|----|----|
| **Item** | **Definition** |
| Aquatic animal | Freshwater, demersal, pelagic and other marine fish; crustaceans, cephalopods and other molluscs; meat aquatic mammals and other aquatic animals |
| Bovine | Bovine meat |
| Small ruminant | Sheep and goats meat |
| Pork | Pork meat |
| Poultry | Poultry meat |
| Eggs | Eggs |
| Dairy | Dairy products |
| Grass | From permanent meadows and pastures |
| Grass-like forage | From temporary meadows and pastures (mixed grass and ray-grass) |
| Other forages | Cultivated forages (alfalfa, beets, legumes, maize, etc.). |
| Fibers | Jute, jute-like fibres, soft-fibres other, sisal, abaca, hard fibres other, tobacco, rubber and seed cotton |
| Roots and Tuber | Potatoes, cassava, sweet potatoes, yams and other roots |
| Fruits & vegetables | Tomatoes, onions, vegetables other, oranges, mandarines, lemons, limes, grapefruit, citrus other, bananas, plantains, apples, pineapples, dates, grapes and other fruits |
| Maize | Maize |
| Wheat | Wheat |
| Rice | Rice, paddy equivalent |
| Other cereals | Barley, rye, oats, millet, sorghum and other cereals |
| Pulses | Beans, peas and other pulses |
| Soyabeans | Soyabeans |
| Soyabean Cake | Soyabean cake |
| Soyabean Oil | Soyabean oil |
| Sunflowerseed | Sunflowerseed |
| Sunflowerseed Cake | Sunflowerseed cake |
| Sunflowerseed Oil | Sunflowerseed oil |
| Rape and Mustardseed | Rape and mustardseed |
| Rape and Mustard Cake | Rape and mustard cake |
| Rape and Mustard Oil | Rape and mustard oil |
| Other Oilcrops | Groundnuts (shelled eq), coconuts – incl copra, sesameseed, olives and other oilcrops |
| Cake Other Oilcrops | Other oilcrops cake |
| Oil Other Oilcrops | Other oilcrops oil |
| Oilpalm fruit | Oilpalm fruit |
| Palmkernel Cake | Palm kernel cake |
| Palm Products Oil | Palm oil and palmkernel oil |
| Sugar | Sugar cane, sugar beet (sugar in equivalent sugar cane and beet) |
| Other plant products | Nuts, coffee, cocoa beans, tea, pepper, pimento, cloves, spices, other |
| Crop residues | Stover |
| Other products | Meat other, offals edible, fats animals raw, honey, meat meal, aquatic plants |
| Occasional | Food leftovers, cut-and-carry, forages and legumes, roadside grasses |

**Herrero et al. (2013) :** source used to complete the FAOstat data, particularly animal feed

**Mueller and al. (2012) :** Source used for the estimation of yields

# Glossary

| Element | Definition |
|----|----|
| Area harvested *(FAO definition)* | Data refer to the area from which a crop is gathered. Area harvested, therefore, excludes the area from which, although sown or planted, there was no harvest due to damage, failure, etc. It is usually net for temporary crops and some times gross for permanent crops. Net area differs from gross area insofar as the latter includes uncultivated patches, footpaths, ditches, headlands, shoulders, shelterbelts, etc.If the crop under consideration is harvested more than once during the year as a consequence of successive cropping (i.e. the same crop is sown or planted more than once in the same field during the year), the area is counted as many times as harvested. On the contrary, area harvested will be recorded only once in the case of successive gathering of the crop during the year from the same standing crops. With regard to mixed and associated crops, the area sown relating to each crop should be reported separately. When the mixture refers to particular crops, generally grains, it is recommended to treat the mixture as if it were a single crop; therefore, area sown is recorded only for the crop reported. |
| Domestic supply quantity *(FAO definition)* | Production + imports - exports + changes in stocks (decrease or increase) = supply for domestic utilization in the new methodology. There are various ways of defining supply and, in fact, various concepts are in use. The elements involved are production, imports, exports and changes in stocks (increase or decrease). There is no doubt that production, imports and stock changes (either decrease or increase in stocks) are genuine supply elements. |
| Feed *(FAO definition)* | Data refer to the quantity of the commodity in question available for feeding to the livestock and poultry during the reference period, whether domestically produced or imported. |
| Food *(FAO definition)* | Data refer to the total amount of the commodity available as human food during the reference period. Data include the commodity in question, as well as any commodity derived therefrom as a result of further processing. Food from maize, for example, comprises the amount of maize, maize meal and any other products derived therefrom available for human consumption. Food from milk relates to the amounts of milk as such, as well as the fresh milk equivalent of dairy products. |
| Losses *(FAO definition)* | Amount of the commodity in question lost through wastage (waste) during the year at all stages between the level at which production is recorded and the household, i.e. storage and transportation. Losses occurring before and during harvest are excluded. Waste from both edible and inedible parts of the commodity occurring in the household is also excluded. Quantities lost during the transformation of primary commodities into processed products are taken into account in the assessment of respective extraction/conversion rates. Distribution wastes tend to be considerable in countries with hot humid climate, difficult transportation and inadequate storage or processing facilities. This applies to the more perishable foodstuffs, and especially to those which have to be transported or stored for a long time in a tropical climate. Waste is often estimated as a fixed percentage of availability, the latter being defined as production plus imports plus stock withdrawals. |
| Food supply *(FAO definition)* | Refers to the total amount of food available for human consumption expressed in kilocalories (kcal). Caloric content is derived by applying the appropriate food composition factors to the quantities of the commodities. |
| LSU (Livestock Unit) |  |

*FAO definitions come from the FAOstat portal* ([https://www.fao.org](https://www.fao.org/faostat/en/#definitions))

Agricultural products (in the category of items) can be aggregated differently according to the elements they describe (cultivated area, food availability, yield). Below we provide details on these aggregations used in GlobAgri :

| Items (within Area harvested element) | Definition |
|----|----|
| Millet and Sorghum |  |
| Oilcrops |  |
| Other | Fibers + fruits and vegetables |
| Other Cereals |  |
| Pulses and Soybeans |  |
| Rice | Rice, paddy equivalent |
| Roots and Tuber |  |
| Tea, cocoa, coffee, oilpalm, sugar cane | All the perennial plants + stimulants (tobacco, rubber...) ==\> to be renamed |
| Wheat | Wheat |

| Unit | Definition |
|----|----|
| TLU (Tropical Livestock Unit) | The tropical livestock unit is commonly taken to be an animal of 250 kg liveweight |

# Bibliographie

Herrero, M., Havlík, P., Valin, H., Notenbaert, A., Rufino, M. C., Thornton, P. K., Blümmel, M., Weiss, F., Grace, D., & Obersteiner, M. (2013). Biomass use, production, feed efficiencies, and greenhouse gas emissions from global livestock systems. *Proceedings Of The National Academy Of Sciences*, *110*(52), 20888‑20893. <https://doi.org/10.1073/pnas.1308149110>

Mueller, N. D., Gerber, J. S., Johnston, M., Ray, D. K., Ramankutty, N., & Foley, J. A. (2012). Closing yield gaps through nutrient and water management. *Nature*, *490*(7419), 254‑257. <https://doi.org/10.1038/nature11420>

==\> Pour en savoir plus sur le modèle et sa construction, voir le lien suivant : [http://agritop.cirad.fr](#0 "http://agritop.cirad.fr")
