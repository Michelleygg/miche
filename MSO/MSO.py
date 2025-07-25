#Install packages
#pip install streamlit plotly pandas folium streamlit_folium openpyxl

import pandas as pd
import streamlit as st
import plotly.express as px
import folium
from folium.plugins import HeatMap
from pyproj import Transformer
from streamlit_folium import st_folium

#Load data
df = pd.read_excel('data/Bird Nuisance (2025)(Updated).xlsx')

#Remove na rows and format datetime
df = df.dropna()
df['Date'] = df['Received Date and Time'].dt.date.astype(str)
df['Month'] = df['Received Date and Time'].dt.to_period('M').astype(str)
df['Time'] = df['Received Date and Time'].dt.time.astype(str)

df['Description'] = df['Description'].apply(str)

#Nature of Issue
def categorize_issue(desc):
    desc = desc.lower()
    if 'feed' in desc:
        return 'Feeding'
    elif 'noise' in desc or 'noisy' in desc or 'caw' in desc:
        return 'Noise'
    elif 'dropping' in desc or 'dirty' in desc or 'clean' in desc:
        return 'Cleanliness'
    else:
        return 'Other'

#Pestbird Type
def extract_pestbird(text):
    birds = ['pigeon', 'myna', 'crow', 'starling', 'sparrow']
    text = text.lower()
    for b in birds:
        if b in text:
            return b
    return 'unknown'

df['Pestbird'] = df['Description'].fillna('') + ' ' + df['Description'].fillna('')
df['Pestbird'] = df['Pestbird'].apply(extract_pestbird)

df['Nature'] = df['Description'].fillna('') + ' ' + df['Description'].fillna('')
df['Nature'] = df['Nature'].apply(categorize_issue)


#Streamlit Page
st.set_page_config(layout="wide")
st.title("Singapore Pestbird Feedback Dashboard (2023–2024)")

#Display df
st.subheader("Data Sample")
st.dataframe(df)

#Chart 1
st.subheader("1. Total Case Count From 1 Jan 2023 to 31 Dec 2024")
feedback_by_month = df.groupby('Month').size().reset_index(name='count')
fig1 = px.line(feedback_by_month, x='Month', y='count', markers=True)
st.plotly_chart(fig1, use_container_width=True)

#Chart 2
st.subheader("2. Feedback by Time of Day")
df['Hour'] = pd.to_datetime(df['Time'], format='%H:%M:%S').dt.hour
feedback_by_hour = df.groupby('Hour').size().reset_index(name='count')
fig2 = px.bar(feedback_by_hour, x='Hour', y='count', labels={'Hour': 'Hour of Day', 'count': 'Number of Feedbacks'},
              title="Number of Feedbacks by Hour of the Day")
st.plotly_chart(fig2, use_container_width=True)

#Chart 3
st.subheader("3. Pestbird Type Over Time")
df3 = df[df['Pestbird'] != 'unknown']
fig3 = px.histogram(df3, x='Month', color='Pestbird', barmode='group')
st.plotly_chart(fig3, use_container_width=True)

#Chart 4
st.subheader("4. Nature of Issue Over Time")
df4 = df[df['Nature'] != 'Other']
fig4 = px.histogram(df4, x='Month', color='Nature', barmode='group')
st.plotly_chart(fig4, use_container_width=True)

#Chart 5
st.subheader("5. Relation Between Pestbird Type and Nature of Issue")
df5 = df[(df['Pestbird'] != 'unknown') & (df['Nature'] != 'Other')]
cross_tab = pd.crosstab(df5['Pestbird'], df5['Nature']).reset_index()
fig5 = px.imshow(cross_tab.set_index('Pestbird'), text_auto=True,
                 color_continuous_scale='YlOrRd', aspect='auto')
st.plotly_chart(fig5, use_container_width=True)

#Chart 6
st.subheader("6. Pestbird Feedback Hotspots")
pestbird_options = df['Pestbird'].unique().tolist()
pestbird_filter = st.multiselect('Select Pestbird Type', pestbird_options, default=pestbird_options)
nature_options = df['Nature'].unique().tolist()
nature_filter = st.multiselect('Select Nature of Issue', nature_options, default=nature_options)

df6 = df[(df['Pestbird'].isin(pestbird_filter)) & (df['Nature'].isin(nature_filter))]
transformer = Transformer.from_crs("EPSG:3414", "EPSG:4326", always_xy=True)
df_coords = df6.dropna(subset=['X Coord', 'Y Coord']).copy()
df_coords[['longitude', 'latitude']] = df_coords.apply(
    lambda row: pd.Series(transformer.transform(row['X Coord'], row['Y Coord'])),
    axis=1)
df_coords = df_coords[(df_coords['latitude'] > 1) & (df_coords['latitude'] < 2)]
m = folium.Map(location=[1.3521, 103.8198], zoom_start=11)
heat_data = df_coords[['latitude', 'longitude']].values.tolist()
HeatMap(heat_data, radius=10).add_to(m)
st_folium(m, width=900, height=600)