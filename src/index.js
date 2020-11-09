import "./main.css";
import "./elm-date-picker.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";
import { db, dateConverter, increment } from "./services/firebase";

const app = Elm.Main.init({
  node: document.getElementById("root"),
});

const getTimezoneOffsetString = () => {
  const timezoneOffsetInMinutes = new Date().getTimezoneOffset();
  const mathematicalSign = timezoneOffsetInMinutes > 0 ? "-" : "+";
  const timezoneOffsetInHours = String(Math.abs(timezoneOffsetInMinutes) / 60).padStart(2, "0");
  return `GMT${mathematicalSign}${timezoneOffsetInHours}:00`;
};

const now = () => new Date(Date.now());

// @todo refactor
const getNumberToIncrement = (previousVal, newVal) => {
  if (previousVal === newVal) {
    return 0;
  }

  if (previousVal) {
    return -1;
  }

  return 1;
};

// @todo refactor
const updateCustomer = (id, customer) => {
  const docRef = db.collection("customers").withConverter(dateConverter).doc(id);
  const statisticsRef = db.collection("meta").doc("statistics");
  return db.runTransaction(async (transaction) => {
    const currentCustomer = await transaction.get(docRef);
    const previousMadeAPurchase = currentCustomer.data().madeAPurchase;
    const newMadeAPurchase = customer.madeAPurchase;
    const numberToIncrement = getNumberToIncrement(previousMadeAPurchase, newMadeAPurchase);
    await transaction.update(docRef, {
      ...customer,
      updatedAt: now(),
    });
    await transaction.update(statisticsRef, {
      madeAPurchase: increment(numberToIncrement),
    });
    return docRef;
  });
};

// @todo refactor
const createCustomer = (customer) => {
  const batch = db.batch();
  const customerRef = db.collection("customers").withConverter(dateConverter).doc();
  const statisticsRef = db.collection("meta").doc("statistics");
  batch.set(customerRef, {
    ...customer,
    createdAt: now(),
    updatedAt: now(),
  });
  batch.update(statisticsRef, {
    total: increment(1),
    madeAPurchase: customer.madeAPurchase ? increment(1) : increment(0),
  });
  return batch.commit().then(() => customerRef);
};

const createOrUpdateCustomer = (id, customer) => {
  if (id) {
    return updateCustomer(id, customer);
  }
  return createCustomer(customer);
};

app.ports.saveCustomer.subscribe(async (customer) => {
  try {
    const { id: maybeId, ...body } = customer;

    const sanitizeCustomerFromElmApp = (body) => ({
      ...body,
      date: new Date(`${body.date} ${getTimezoneOffsetString()}`),
    });

    const sanitizedCustomer = sanitizeCustomerFromElmApp(body);
    const { id } = await createOrUpdateCustomer(maybeId, sanitizedCustomer);

    app.ports.saveCustomerSuccess.send({
      message: id,
    });
  } catch (e) {
    app.ports.saveCustomerError.send({
      message: e.message,
    });
  }
});

const sanitizeCustomerForElmApp = (customer) => {
  const { id } = customer;

  const sanitizeCustomer = (body) => ({
    id,
    ...body,
    date: body.date.toISOString(),
  });

  return sanitizeCustomer(customer.data());
};

app.ports.loadCustomer.subscribe(async (id) => {
  try {
    const customer = await db.collection("customers").withConverter(dateConverter).doc(id).get();

    if (customer.exists) {
      const sanitizedCustomer = sanitizeCustomerForElmApp(customer);
      app.ports.loadCustomerSuccess.send(sanitizedCustomer);
    } else {
      throw new Error("Customer does not exist.");
    }
  } catch (e) {
    app.ports.loadCustomerError.send({
      message: e.message,
    });
  }
});

app.ports.loadCustomerList.subscribe(async () => {
  try {
    await db.collection("customers")
      .withConverter(dateConverter)
      .orderBy("date")
      .onSnapshot((customerList) => {
        const sanitizedCustomerList = customerList.docs.map(sanitizeCustomerForElmApp);
        app.ports.loadCustomerListSuccess.send(sanitizedCustomerList);
      });
  } catch (e) {
    app.ports.loadCustomerListError.send({
      message: e.message,
    });
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
