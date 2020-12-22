# app.py
from flask import Flask, render_template, request
from werkzeug.utils import redirect  
from flask_wtf import FlaskForm
from wtforms import SelectField, FloatField
import os
import pickle
from sklearn.linear_model import LogisticRegression
import joblib
import pandas as pd
from sklearn.preprocessing import OrdinalEncoder, MinMaxScaler
import numpy as np
app = Flask(__name__) 
SECRET_KEY = os.urandom(32)
app.config['SECRET_KEY'] = SECRET_KEY

model = joblib.load('models/finalized_modelLogisticRegression.joblib')

class Form(FlaskForm):
    state = SelectField(choices=['DC', 'KY', 'ME', 'MS', 'TN', 'PA', 'CT', 'ND', 'KS', 'OH', 'NJ', 'OK', 'AL', 'MA', 'MO', 'LA', 'WV' , 'IN' , 'RI' , 'IA',
                                 'MT' , 'NY', 'ID', 'VT', 'VA','TX', 'FL', 'CO', 'AZ', 'SC', 'NE', 'WY', 'HI', 'IL', 'NH', 'GA', 'AK', 'MD', 'AR', 'WI', 
                                 'OR', 'MI', 'DE', 'UT', 'CA', 'MN', 'SD', 'NC', 'WA', 'NM', 'NV'])
    account_length = FloatField()
    area_code = SelectField(choices=['area_code_415', 'area_code_408', 'area_code_510'])
    international_plan = SelectField(choices=['no', 'yes'])
    voice_mail_plan = SelectField(choices=['no', 'yes'])
    number_vmail_messages = FloatField()
    total_day_minutes = FloatField()
    total_day_calls = FloatField()
    total_day_charge = FloatField()
    total_eve_minutes = FloatField()
    total_eve_calls = FloatField()
    total_eve_charge = FloatField()
    total_night_minutes = FloatField()
    total_night_calls = FloatField()
    total_night_charge = FloatField()
    total_intl_minutes = FloatField()
    total_intl_calls = FloatField()
    total_intl_charge = FloatField()
    number_customer_service_calls = FloatField()

@app.route('/')
def inicio():
    form = Form()
    return render_template('index.html', form = form)

@app.route('/predict', methods=['POST'])
def prever():
    form = Form()
    unnamed = 0
    state = form.state.data
    account_length = form.account_length.data
    area_code = form.area_code.data 
    international_plan = form.international_plan.data 
    voice_mail_plan = form.voice_mail_plan.data 
    number_vmail_messages = form.number_vmail_messages.data 
    total_day_minutes = form.total_day_minutes.data 
    total_day_calls = form.total_day_calls.data 
    total_day_charge = form.total_day_charge.data 
    total_eve_minutes = form.total_eve_minutes.data
    total_eve_calls = form.total_eve_calls.data 
    total_eve_charge = form.total_eve_charge.data 
    total_night_minutes = form.total_night_minutes.data 
    total_night_calls = form.total_night_calls.data 
    total_night_charge = form.total_night_charge.data 
    total_intl_minutes = form.total_intl_minutes.data 
    total_intl_calls = form.total_intl_calls.data 
    total_intl_charge = form.total_intl_charge.data 
    number_customer_service_calls = form.number_customer_service_calls.data
    dic = {'Unnamed: 0': [unnamed], 'state': [state], 'account_length': [account_length], 'area_code': [area_code], 'international_plan': [international_plan],
    'voice_mail_plan': [voice_mail_plan], 'number_vmail_messages': [number_vmail_messages], 'total_day_minutes': [total_day_minutes], 'total_day_calls': [total_day_calls],
    'total_day_charge': [total_day_charge], 'total_eve_minutes': [total_eve_minutes], 'total_eve_calls': [total_eve_calls], 'total_eve_charge': [total_eve_charge],
    'total_night_minutes': [total_night_minutes], 'total_night_calls': [total_night_calls], 'total_night_charge': [total_night_charge], 'total_intl_minutes': [total_intl_minutes],
    'total_intl_calls': [total_intl_calls], 'total_intl_charge': [total_intl_charge], 'number_customer_service_calls': [number_customer_service_calls]}
    dados = pd.DataFrame.from_dict(dic)

    # Limpando dados
    # Dropando coluna unnamed: 0
    dados.drop('Unnamed: 0', axis = 1, inplace = True)
    # OrdinalEncoder, é usado para codificar atributos categóricos como um integer array
    categoric_features = ['state', 'area_code', 'international_plan', 'voice_mail_plan']
    oe = OrdinalEncoder()
    for column in categoric_features:
        print(column)
        oe.categories_ = np.load('models/' + str(column) + '_OrdinalEncoder.npy', allow_pickle=True)
        dados[column] = oe.transform(np.array(dados[column]).reshape(-1, 1))
    # Carregando o MinMaxScaler
    scaler = MinMaxScaler()
    # Ajustando valores normalizados nas colunas e salvando os modelos
    attributes = dados.columns
    for column in attributes:
        scaler = joblib.load('models/'+ str(column) +'MinMaxScaller.mod')  
        dados[column] = scaler.transform(np.array(dados[column]).reshape(-1, 1))
    predict = model.predict(np.array(dados))
    predict_prob = model.predict_proba(np.array(dados))
    if predict == 1:
        return 'O cliente irá dá churn com probabilidade de ' + str(predict_prob[0][1] * 100)  + '%.'
    else:
        return 'O cliente não dará churn.'


app.run(debug = True)