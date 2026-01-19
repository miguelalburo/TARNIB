import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler, OneHotEncoder, LabelEncoder
from tensorflow.keras.preprocessing.sequence import pad_sequences
from tensorflow.keras.models import Model
from tensorflow.keras.layers import Input, LSTM, Dense, Masking, Embedding, Concatenate
from tensorflow.keras.utils import to_categorical

# ============================
# 1️⃣ Load Data
# ============================

train_df = pd.read_csv("../data/train.csv") # Training Set
test_df = pd.read_csv("../data/test.csv") # Testing Set

# Sort by subject and admission
train_df = train_df.sort_values(by=['subject_id', 'admission_id'])

# ============================
# 2️⃣ Scaling 
# ============================

# Normalise Age
scaler_age = MinMaxScaler()
train_df['age_scaled'] = scaler_age.fit_transform(train_df[['age']])
test_df['age_scaled'] = scaler_age.transform(test_df['age'])


# Normalize Length of Stay
scaler_los = MinMaxScaler()
train_df['LoS_scaled'] = scaler_los.fit_transform(train_df[['LoS']])
test_df['LoS_scaled'] = scaler_los.fit_transform(test_df[['LoS']])

# Normalize Length of Stay
scaler_erlos = MinMaxScaler()
train_df['ER_LoS_scaled'] = scaler_erlos.fit_transform(train_df[['ER_LoS']])
test_df['ER_LoS_scaled'] = scaler_erlos.fit_transform(test_df[['ER_LoS']])

# ============================
# 4️⃣ Build sequences per subject
# ============================

def build_sequences(df, feature_cols, target_col):
    subjects = df['subject_id'].unique()
    sequences = []
    targets = []
    
    for subj in subjects:
        subj_data = df[df['subject_id'] == subj].sort_values(by='admission_id')
        seq_features = subj_data[feature_cols].values
        seq_target = subj_data[target_col].values
        
        sequences.append(seq_features)
        targets.append(seq_target)
    
    # Pad sequences to max length
    max_seq_len = max(len(s) for s in sequences)
    X_padded = pad_sequences(sequences, maxlen=max_seq_len, dtype='float32', padding='pre', truncating='pre')
    y_padded = pad_sequences(targets, maxlen=max_seq_len, dtype='float32', padding='pre', truncating='pre')
    
    return X_padded, y_padded, max_seq_len

# Suppose these are all columns in your dataframe
all_columns = train_df.columns.tolist()
exclude_features = ['subject_id', 'hadm_id', 'hospital_expire_flag', 'days_until_death']
feature_cols = [c for c in all_columns if c not in exclude_features]

target_col = 'hospital_expire_flag'

X_train, y_train, max_seq_len = build_sequences(train_df, feature_cols, target_col)
X_test, y_test, _ = build_sequences(test_df, feature_cols, target_col)

# ============================
# 5️⃣ Build LSTM model
# ============================

num_features = X_train.shape[2]

inputs = Input(shape=(max_seq_len, num_features))
x = Masking(mask_value=0.0)(inputs)  # Ignore padded timesteps
x = LSTM(64)(x)  # Return output of last timestep
outputs = Dense(1, activation='sigmoid')(x)

model = Model(inputs, outputs)
model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])

model.summary()

# ============================
# 6️⃣ Train model
# ============================

# Optionally handle class imbalance
from sklearn.utils import class_weight

class_weights = class_weight.compute_class_weight(
    class_weight='balanced',
    classes=np.unique(y_train[:, -1]),
    y=y_train[:, -1]
)
class_weights_dict = {i: w for i, w in enumerate(class_weights)}

model.fit(
    X_train, y_train[:, -1],
    validation_data=(X_test, y_test[:, -1]),
    epochs=10,
    batch_size=32,
    class_weight=class_weights_dict
)
