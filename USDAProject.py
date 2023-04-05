import pandas as pd


med_age = pd.read_csv('median-age.csv')
no_educ = pd.read_csv('share-of-population-15-years-and-older-with-no-education.csv')
gdp_per_cap = pd.read_csv('gdp per capita.csv')

potatoes = pd.read_csv('US Potato Exports.csv')
potatoes = potatoes.transpose()
potatoes.columns = potatoes.iloc[0]
potatoes = potatoes[1:].reset_index(drop=False)
potatoes = potatoes.rename(
        columns={'index': 'Year'})

print(potatoes)
potatoes.to_csv('US Potato Exports transposed.csv',index=False)

"""
## didn't realize I messed up the average gdp per capita thing for the poor countries, fixing it here
low_income_countries = ['Burundi', 'Central African Republic', 'Syrian Arab Republic', 'Madagascar', 'Congo, Dem. Rep.',
                        'Niger', 'Gambia, The', 'Sudan', 'Rwanda', 'Uganda']

gdp_df = pd.DataFrame(data={'Year':range(1950,2022)})
for country in low_income_countries:
    print(country)
    country_df = gdp_per_cap[gdp_per_cap['Country Name'] == country].transpose().reset_index(drop=False)
    country_df = country_df[4:len(country_df) - 1]
    print(country_df)
    country_df = country_df.rename(
        columns={'index': 'Year', country_df.columns[1]: '{0} GDP Per Capita'.format(country)})
    country_df['Year'] = country_df['Year'].astype(int)

    gdp_df = gdp_df.merge(country_df, how='outer')
    print(gdp_df)

gdp_df.to_csv('low-income-gdp-per-capita.csv',index=False)
"""
"""
us_df = pd.DataFrame(data={'Year':range(1950, 2022)})

us_age = med_age[med_age['Entity'] == 'United States'][['Year','Median age - Sex: all - Age: all - Variant: estimates']].dropna().reset_index(drop=True)
us_age = us_age.rename(
    columns={'Median age - Sex: all - Age: all - Variant: estimates': 'US Median Age'})
us_df = us_df.merge(us_age, how='outer')

us_no_educ = no_educ[no_educ['Entity'] == 'United States'][['Year','Barro-Lee: Percentage of population age 15+ with no education']].dropna().reset_index(drop=True)
us_no_educ = us_no_educ.rename(
    columns={'Barro-Lee: Percentage of population age 15+ with no education': 'US % of Pop 15+ with no education'})
us_df = us_df.merge(us_no_educ, how='outer')

us_gdp_per_cap = gdp_per_cap[gdp_per_cap['Country Name'] == 'United States'].transpose().reset_index(drop=False)
us_gdp_per_cap = us_gdp_per_cap[4:len(us_gdp_per_cap)-1]
us_gdp_per_cap = us_gdp_per_cap.rename(
    columns={'index': 'Year',251:'US GDP Per Capita'})
us_gdp_per_cap['Year'] = us_gdp_per_cap['Year'].astype(int)
print(type(us_gdp_per_cap['Year'].iloc[1]))
us_df = us_df.merge(us_gdp_per_cap, how='outer')
print(us_gdp_per_cap)
print('-----------------------------------------------------------------------')

print(us_df)
us_df.to_csv('us-economic-df.csv',index=False)
"""

"""
low_income_countries = ['Burundi', 'Central African Republic', 'Syria', 'Madagascar', 'Democratic Republic of Congo',
                        'Niger', 'Gambia', 'Sudan', 'Rwanda', 'Uganda']

milk = pd.read_csv('global-food (milk).csv')
potatoes = pd.read_csv('global-food (potatoes).csv')
rice = pd.read_csv('global-food (rice).csv')
wheat = pd.read_csv('global-food (wheat).csv')
maize = pd.read_csv('global-food (maize).csv')

low_income_milk = pd.DataFrame(data={'Year': range(1950, 2022)})
low_income_potatoes = pd.DataFrame(data={'Year': range(1950, 2022)})
low_income_rice = pd.DataFrame(data={'Year': range(1950, 2022)})
low_income_wheat = pd.DataFrame(data={'Year': range(1950, 2022)})
low_income_maize = pd.DataFrame(data={'Year': range(1950, 2022)})

for country in low_income_countries:
    #country_milk = milk[milk['Country'] == country][['Year', 'Production (t)']].reset_index(drop=True)
    country_milk = milk[milk['Country'] == country][['Year', 'Food supply (kg per capita per year)']].reset_index(drop=True)
    #country_milk = country_milk.rename(columns={'Production (t)':'{0} Milk Production in tons'.format(country)})
    country_milk = country_milk.rename(columns={'Food supply (kg per capita per year)': '{0} Milk Supply (kg per capita)'.format(country)})
    low_income_milk = low_income_milk.merge(country_milk, how='outer')

    #country_potatoes = potatoes[potatoes['Country'] == country][['Year', 'Production (t)']].reset_index(drop=True)
    country_potatoes = potatoes[potatoes['Country'] == country][['Year', 'Food supply (kg per capita per year)']].reset_index(drop=True)
    #country_potatoes = country_potatoes.rename(columns={'Production (t)': '{0} Potato Production in tons'.format(country)})
    country_potatoes = country_potatoes.rename(columns={'Food supply (kg per capita per year)': '{0} Potato Supply (kg per capita)'.format(country)})
    low_income_potatoes = low_income_potatoes.merge(country_potatoes, how='outer')

    #country_rice = rice[rice['Country'] == country][['Year', 'Production (t)']].reset_index(drop=True)
    country_rice = rice[rice['Country'] == country][['Year', 'Food supply (kg per capita per year)']].reset_index(drop=True)
    #country_rice = country_rice.rename(columns={'Production (t)': '{0} Rice Production in tons'.format(country)})
    country_rice = country_rice.rename(columns={'Food supply (kg per capita per year)': '{0} Rice Supply (kg per capita)'.format(country)})
    low_income_rice = low_income_rice.merge(country_rice, how='outer')

    #country_wheat = wheat[wheat['Country'] == country][['Year', 'Production (t)']].reset_index(drop=True)
    country_wheat = wheat[wheat['Country'] == country][['Year', 'Food supply (kg per capita per year)']].reset_index(drop=True)
    #country_wheat = country_wheat.rename(columns={'Production (t)': '{0} Wheat Production in tons'.format(country)})
    country_wheat = country_wheat.rename(columns={'Food supply (kg per capita per year)': '{0} Wheat Supply (kg per capita)'.format(country)})
    low_income_wheat = low_income_wheat.merge(country_wheat, how='outer')

    #country_maize = maize[maize['Country'] == country][['Year', 'Production (t)']].reset_index(drop=True)
    country_maize = maize[maize['Country'] == country][['Year', 'Food supply (kg per capita per year)']].reset_index(drop=True)
    #country_maize = country_maize.rename(columns={'Production (t)': '{0} Maize Production in tons'.format(country)})
    country_maize = country_maize.rename(columns={'Food supply (kg per capita per year)': '{0} Maize Supply (kg per capita)'.format(country)})
    low_income_maize = low_income_maize.merge(country_maize, how='outer')

low_income_milk.to_csv('low-income-milk-supply.csv',index=False)
low_income_potatoes.to_csv('low-income-potatoes-supply.csv',index=False)
low_income_rice.to_csv('low-income-rice-supply.csv',index=False)
low_income_wheat.to_csv('low-income-wheat-supply.csv',index=False)
low_income_maize.to_csv('low-income-maize-supply.csv',index=False)
"""



"""low_income_age = pd.DataFrame(data={'Year': range(1950, 2022)})
low_income_no_educ = pd.DataFrame(data={'Year': range(1950, 2022)})
count = 0
for country in low_income_countries:
    low_income_age['{0} Median Age'.format(country)] = med_age[med_age['Entity'] == country]['Median age - Sex: all - Age: all - Variant: estimates'].reset_index(drop=True)
    #print(low_income_age)
    low_income_age = low_income_age.dropna()
    #print(low_income_age)

    country_no_educ = no_educ[no_educ['Entity'] == country][['Year','Barro-Lee: Percentage of population age 15+ with no education']].reset_index(drop=True)
    country_no_educ = country_no_educ.rename(columns={'Barro-Lee: Percentage of population age 15+ with no education':'{0} % of Pop 15+ with no education'.format(country)})
    #print(country, '\n', country_no_educ)
    low_income_no_educ = low_income_no_educ.merge(country_no_educ, how='outer')

    count += 1


low_income_age.to_csv('low-income-age.csv',index=False)
low_income_no_educ.to_csv('low-income-no-education.csv',index=False)
"""


