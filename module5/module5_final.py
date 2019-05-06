from flask import Flask, jsonify
import pandas as pd
import json

app = Flask(__name__)

spec_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common' +\
        '&$group=spc_common' +\
        '&$order=spc_common').replace(' ', '%20')
species = pd.read_json(spec_url)

boroughs = ['Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island']
start = 'https://data.cityofnewyork.us/resource/nwxe-4ae8.json?$select=boroname,spc_common,health,steward,count(tree_id)&$where=&boroname=%27'
end = '%27&$group=boroname,spc_common,health,steward&$order=boroname,spc_common,health,steward'

soql_urls = [(start + boro + end).replace(' ', '%20') for boro in boroughs]

bx_trees = pd.read_json(soql_urls[0])
bkln_trees = pd.read_json(soql_urls[1])
man_trees = pd.read_json(soql_urls[2])
qns_trees = pd.read_json(soql_urls[3])
si_trees = pd.read_json(soql_urls[4])

all_trees = pd.concat([bx_trees, bkln_trees,  man_trees,  qns_trees, si_trees], ignore_index=True)

# return borough list and species list on home page

@app.route('/')
def return_lists():
    instructions = {'Instructions': 'Add /NYC or any borough name to the end of the url to see all trees in that location.  Add /speciesname after the borough to select one specific species in that location'}
    boros_dict = {'borough names': boroughs}
    species_dict = {'species names': list(species['spc_common'])}
    return json.dumps([instructions, boros_dict, species_dict])

# return counts for all trees in the city or in one borough respectively

@app.route('/NYC')
def return_all():
    trees = all_trees.to_json(orient='records')
    return trees

@app.route('/Bronx')
def return_bx():
    trees = bx_trees.to_json(orient='records')
    return trees

@app.route('/Brooklyn')
def return_bkln():
    trees = bkln_trees.to_json(orient='records')
    return trees

@app.route('/Manhattan')
def return_man():
    trees = man_trees.to_json(orient='records')
    return trees

@app.route('/Bronx')
def return_qns():
    trees = qns_trees.to_json(orient='records')
    return trees

@app.route('/StatenIsland')
def return_si():
    trees = si_trees.to_json(orient='records')
    return trees

# return only specific species for whole city or one borough

@app.route('/NYC/<string:species>')
def return_species_all(species):
    trees = all_trees[all_trees['spc_common'] == species].to_json(orient='records')
    return trees

@app.route('/Bronx/<string:species>')
def return_species_bx(species):
    trees = bx_trees[bx_trees['spc_common'] == species].to_json(orient='records')
    return trees

@app.route('/Brooklyn/<string:species>')
def return_species_bkln(species):
    trees = bkln_trees[bkln_trees['spc_common'] == species].to_json(orient='records')
    return trees

@app.route('/Manhattan/<string:species>')
def return_species_man(species):
    trees = man_trees[man_trees['spc_common'] == species].to_json(orient='records')
    return trees

@app.route('/Bronx/<string:species>')
def return_species_qns(species):
    trees = qns_trees[qns_trees['spc_common'] == species].to_json(orient='records')
    return trees

@app.route('/StatenIsland/<string:species>')
def return_species_si(species):
    trees = si_trees[si_trees['spc_common'] == species].to_json(orient='records')
    return trees

if __name__ == '__main__':
    app.run(debug=True)
