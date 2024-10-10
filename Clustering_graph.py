import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from tabulate import tabulate
from PIL import Image, ImageDraw, ImageFont
import urllib.request


df = pd.read_excel('FSExtract_Norm_Final.xlsx')

pd.set_option('display.max_columns', 15)

#--------------------------------------------------------------------------------

# Statistics

#--------------------------------------------------------------------------------

df.dropna(inplace=True)




# print the number of rows and columns in the dataframe
print(df.shape)

# print the data types and number of non-null values in each column
print(df.info())

# print summary statistics of the numerical column
print(df.describe())

# print the first few rows of the dataframe
print(df.head())


#--------------------------------------------------------------------------------

# Legal Form

#--------------------------------------------------------------------------------


legal_form_value_counts = df['LegalForm'].value_counts()
proportions_legal_form = legal_form_value_counts / df.shape[0] * 100

# Create a dataframe with the value counts and proportions
df_legal_form = pd.concat([legal_form_value_counts, proportions_legal_form], axis=1)
df_legal_form.columns = ['Counts', 'Proportions']

# Format the proportions as percentages
df_legal_form['Proportions'] = df_legal_form['Proportions'].apply(lambda x: '{:.2f}%'.format(x))


# Set the color of the bars to light green
color = ['#B0C4DE']

# Create a bar chart
plt.bar(legal_form_value_counts.index, legal_form_value_counts, color=color)
plt.xticks(rotation=10)

# Add the proportion as text on the bars
for i, value in enumerate(legal_form_value_counts):
    plt.text(i, value + 0.5, f'{proportions_legal_form[i]:.1f}%', ha='center',  weight = 'bold', size = 10)

# Set the title and labels
plt.title('Legal Form Distribution')
plt.xlabel('Legal Form')
plt.ylabel('Count')

# Show the plot
plt.show()

#--------------------------------------------------------------------------------

# Activity

#--------------------------------------------------------------------------------


activity_value_counts = df['NACEActivity'].value_counts()
proportions_activity = activity_value_counts / df.shape[0] * 100

# Create a dataframe with the value counts and proportions
df_activity = pd.concat([activity_value_counts, proportions_activity], axis=1)
df_activity.columns = ['Counts', 'Proportions']

# Format the proportions as percentages
df_activity['Proportions'] = df_activity['Proportions'].apply(lambda x: '{:.2f}%'.format(x))


# Set the color of the bars to light green
color = ['#B0C4DE']

# Create a bar chart
plt.bar(activity_value_counts.index, activity_value_counts, color=color)
plt.xticks(rotation=30, size = 7)

# Add the proportion as text on the bars
for i, value in enumerate(activity_value_counts):
    plt.text(i, value + 0.5, f'{proportions_activity[i]:.1f}%', ha='center',  weight = 'bold', size = 10)

# Set the title and labels
plt.title('Activity Distribution')
plt.xlabel('Activity')
plt.ylabel('Count')

# Show the plot
plt.show()

#--------------------------------------------------------------------------------

# Filling Date Distribution

#--------------------------------------------------------------------------------

date_value_counts = df['FilingDate'].value_counts()
proportions_date = date_value_counts / df.shape[0] * 100

# Create a dataframe with the value counts and proportions
df_date = pd.concat([date_value_counts, proportions_activity], axis=1)
df_date.columns = ['Counts', 'Proportions']

# Format the proportions as percentages
df_date['Proportions'] = df_date['Proportions'].apply(lambda x: '{:.2f}%'.format(x))


# Select the top 10 values
top10_date_value_counts = date_value_counts.nlargest(10)

# Calculate the proportion of each value
proportions_top10_date = top10_date_value_counts / df.shape[0] * 100

# Set the color of the bars to light green
color = ['#B0C4DE']

# Create a bar chart
plt.bar(top10_date_value_counts.index, top10_date_value_counts, color=color)
plt.xticks(rotation=30)

# Add the proportion as text on the bars
for i, value in enumerate(top10_date_value_counts):
    plt.text(i, value + 0.5, f'{proportions_top10_date[i]:.1f}%', ha='center',  weight = 'bold', size = 10)

# Set the title and labels
plt.title('Date Distribution (Top 10)')
plt.xlabel('Date')
plt.ylabel('Count')

# Show the plot
plt.show()

#--------------------------------------------------------------------------------

# Profit Distribution

#--------------------------------------------------------------------------------


data = df['ProfitLossEUR']

# Create the histogram
n, bins, patches = plt.hist(data, bins=np.arange(-10, 10, 1), color='#B0C4DE')

# Set the labels and title
plt.xlabel('Value')
plt.ylabel('Count')
plt.title('Profit/Loss Distribution (1 Bin = 1 Eur)')

# Add the percentage as text on the bars
for i, p in enumerate(patches):
    height = p.get_height()
    percent = height / sum(n) * 100
    plt.text(p.get_x() + p.get_width() / 2,
             height + 10,
             f'{percent:.1f}%',
             ha='center')

# Show the plot
plt.show()
#--------------------------------------------------------------------------------

# Asset Distribution

#--------------------------------------------------------------------------------

# Create the histogram
fig, ax = plt.subplots()
n, bins, patches = ax.hist(df['TotalAssetsEUR'], bins=np.arange(0, 17, 1), align='mid', color='#B0C4DE')

# Calculate the percentages
total = np.sum(n)
percentages = [f'{100 * count / total:.2f}%' for count in n]

# Add the percentage labels to the bars
for i, patch in enumerate(patches):
    x = patch.get_x() + patch.get_width() / 2
    y = patch.get_height()
    ax.annotate(percentages[i], (x, y), ha='center', va='bottom', size=8)

# Set the title and labels
plt.title('Total Assets EUR Distribution')
plt.xlabel('Total Assets EUR')
plt.ylabel('Count')

# Show the plot
plt.show()




#--------------------------------------------------------------------------------

# violin plot

#--------------------------------------------------------------------------------



df_sub = df[(df['TotalAssetsEUR'] >= 0) & (df['TotalAssetsEUR'] <= 6)]

sns.violinplot(x=df_sub['TotalAssetsEUR'], palette ="Set2")
# Set the title and labels
plt.title('Violin plot : Total Assets (from 0 to 7, 95% of the distribution)')
plt.xlabel('Total Assets')
plt.ylabel()
plt.show()



#--------------------------------------------------------------------------------

# Clustering viz correlation

#--------------------------------------------------------------------------------



# Replace 0 with 3 in the 'Cluster' column
df_final['Cluster'] = df_final['Cluster'].replace(0, 3)

# Define a list of colors, with one color for each cluster
colors = ['#66c2a5', '#fc8d62', '#e78ac3']
# Plot a scatterplot of profit vs assets for each cluster
fig, axes = plt.subplots(1, 3, figsize=(16, 6), sharex=True, sharey=True)

for i, cluster in enumerate(df_final['Cluster'].unique()):
    # Subset the dataframe for the current cluster
    df_cluster = df_final[df_final['Cluster'] == cluster]

    # Plot a scatterplot for the current cluster
    sns.scatterplot(data=df_cluster, x='TotalAssetsEUR', y='ProfitLossEUR', hue='Cluster', palette=[colors[i]], ax=axes[i])
    axes[i].set_xlabel('Total Assets (EUR)')
    axes[i].set_ylabel('Profit/Loss (EUR)')
    axes[i].set_title(f'Cluster {cluster}')

# Add a title to the overall plot
plt.suptitle('Profit vs Assets by Cluster')



import matplotlib.pyplot as plt
import seaborn as sns

# Define a list of colors, with one color for each cluster
colors = ['#66c2a5', '#e78ac3',  '#fc8d62']

sns.scatterplot(data=df_final, x='TotalAssetsEUR', y='ProfitLossEUR', hue='Cluster', palette=colors)
# Set axis labels and title
plt.xlabel('Total Assets (EUR)')
plt.ylabel('Profit/Loss (EUR)')
plt.title('Profit vs Assets by Cluster')


#--------------------------------------------------------------------------------

# Heat map and coordinates paralleles

#--------------------------------------------------------------------------------



import seaborn as sns

sns.heatmap(df_final[['Cluster', 'TotalAssetsEUR', 'ProfitLossEUR', 'NACEActivity', 'LegalForm']].corr(), cmap='Paired', annot=True)
plt.title('HeatMap')



from pandas.plotting import parallel_coordinates

# One-hot encode the NACEActivity and LegalForm columns
df_encoded = pd.get_dummies(df_final, columns=['NACEActivity', 'LegalForm'])

# Create the parallel coordinates plot
plt.figure(figsize=(8,6))
parallel_coordinates(df_encoded[['Cluster', 'TotalAssetsEUR', 'ProfitLossEUR', 'NACEActivity_Financial & Insurance', 'NACEActivity_Professional & Scientific','NACEActivity_Wholesale & Retail', 'NACEActivity_Real estate', 'NACEActivity_Construction']], 'Cluster', colormap='Dark2')
plt.xticks(rotation=30)
plt.title('Parallel Coordinates Plot (86 % of the Activity Distribution)')
plt.show()


