from flask import Flask, jsonify
import pandas as pd
import urllib.request, json

app = Flask(__name__)

# This should return all the Staten Island trees grouped by name, then health, and ordered the same way

soql_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,health,steward,count(tree_id)' +\
        '&$where=&boroname=\''+'Staten Island'+'\'' +\
        '&$group=spc_common,health,steward' +\
        '&$order=spc_common,health,steward').replace(' ', '%20')

all_trees = pd.read_json(soql_url)
#print(json_trees)

# Thought about not using pandas, but then I would have to use loops to filter data
#with urllib.request.urlopen(soql_url) as url:
#    all_trees = json.loads(url.read().decode())


@app.route('/StatenIslandTrees/<string:species>')
def return_species(species):
    trees = all_trees[all_trees['spc_common'] == species].to_json(orient='records')
    return trees

if __name__ == '__main__':
    app.run(debug=True)
