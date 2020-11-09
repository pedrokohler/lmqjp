import * as firebase from "firebase/app";
import "firebase/firebase-firestore";

const firebaseConfig = {
  apiKey: process.env.ELM_APP_FIREBASE_API_KEY,
  authDomain: process.env.ELM_APP_FIREBASE_AUTH_DOMAIN,
  databaseURL: process.env.ELM_APP_FIREBASE_DATABASE_URL,
  projectId: process.env.ELM_APP_FIREBASE_PROJECT_ID,
  storageBucket: process.env.ELM_APP_FIREBASE_STORAGE_BUCKET,
  appId: process.env.ELM_APP_FIREBASE_APP_ID,
};

firebase.initializeApp(firebaseConfig);

export const db = firebase.firestore();

export const increment = (n) => firebase.firestore.FieldValue.increment(n);

const convertObjectDateFields = (convertObjectDate, obj) => {
  const newData = Object.keys(obj).reduce(convertObjectDate, obj);
  return newData;
};

export const dateConverter = {
  fromFirestore(snapshot, options) {
    const data = snapshot.data(options);
    const convertObjectDate = (obj, key) => {
      if (data[key].toDate) {
        return { ...obj, [key]: data[key].toDate() };
      }
      return obj;
    };
    return convertObjectDateFields(convertObjectDate, data);
  },

  toFirestore(data) {
    const convertObjectDate = (obj, key) => {
      if (data[key] instanceof Date) {
        return { ...obj, [key]: firebase.firestore.Timestamp.fromDate(data[key]) };
      }
      return obj;
    };
    return convertObjectDateFields(convertObjectDate, data);
  },
};
