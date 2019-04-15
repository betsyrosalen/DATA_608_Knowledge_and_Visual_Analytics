import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import plotly.graph_objs as go
import pandas as pd
import numpy as np

app = dash.Dash()

### Get List of Species Names
spec_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common' +\
        '&$group=spc_common' +\
        '&$order=spc_common').replace(' ', '%20')
species = pd.read_json(spec_url)

species_list = list(species['spc_common'])
boro_list = ['Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island']

app.layout = html.Div([
    html.Div([

        html.H3('NYC Tree Health by Stewardship'),
        
        html.Div([
            dcc.Dropdown(
                id='chosen_boro',
                options=[{'label': i, 'value': i} for i in boro_list],
                value='Staten Island'
            )
        ],
        style={'width': '48%', 'display': 'inline-block'}),

        html.Div([
            dcc.Dropdown(
                id='chosen_species',
                options=[{'label': i, 'value': i} for i in species_list],
                value='American beech'
            )
        ],style={'width': '48%', 'float': 'right', 'display': 'inline-block'})
    ]),

    dcc.Graph(id='bar_graph')
    
], style={'padding':20})

@app.callback(
    Output('bar_graph', 'figure'),
    [Input('chosen_boro', 'value'),
     Input('chosen_species', 'value')])

def update_graph(chosen_boro, chosen_species):

    url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
            '$select=health,steward,count(tree_id)' +\
            '&$where=spc_common=\''+chosen_species+'\'' +\
            '&boroname=\''+chosen_boro+'\'' +\
            '&$group=spc_common,health,steward' +\
            '&$order=spc_common,steward,health').replace(' ', '%20')
    trees = pd.read_json(url)

    trees.loc[trees['steward'] == 'None', 'steward'] = '0-None'
    trees[['steward', 'health']] = trees[['steward', 'health']].astype('category')
    steward_cats = list(trees['steward'].unique())
    steward_cats.sort()
    trees['steward'] = trees['steward'].cat.reorder_categories(steward_cats)
    trees['health'] = trees['health'].cat.reorder_categories(['Good', 'Fair', 'Poor'])

    props = trees.groupby(['steward', 'health']).agg({'count_tree_id': 'sum'}).groupby(level=0).apply(lambda g: g / g.sum()).reset_index()

    traces = []
    colors = ['YellowGreen ', 'Gold', 'Tomato']
    i=0
    for h in props.health.cat.categories.tolist():
        props_by_health = props[props['health'] == h]
        traces.append(go.Bar(
            x=props_by_health['steward'],
            y=props_by_health['count_tree_id'],
            marker=dict(color=colors[i]),
            name=h
        ))
        i+=1
        
    return {
        'data': traces,
        'layout': go.Layout(
            title='Proportionate Healthy of Trees by Stewardship',
            barmode='stack',
            xaxis={
                'title': 'Steward Activity'
            },
            yaxis={
                'title': 'Proportion of Trees in Good, Fair and Poor Health'
            }
        )
    }

if __name__ == '__main__':
    app.run_server()