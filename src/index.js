import "./main.css";
import "./elm-date-picker.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";
import { db, dateConverter } from "./services/firebase";

const app = Elm.Main.init({
  node: document.getElementById("root"),
});

const getTimezoneOffsetString = () => {
  const timezoneOffsetInMinutes = new Date().getTimezoneOffset();
  const timezoneOffsetInHours = String(Math.abs(timezoneOffsetInMinutes) / 60).padStart(2, "0");
  const mathematicalSign = timezoneOffsetInHours > 0 ? "-" : "+";
  return `GMT${mathematicalSign}${timezoneOffsetInHours}:00`;
};

const now = () => new Date(Date.now());

const updateCustomer = (id, customer) => {
  const docRef = db.collection("customers").withConverter(dateConverter).doc(id);

  return docRef.update({
    ...customer,
    updatedAt: now(),
  }).then(() => docRef);
};

const createCustomer = (customer) => db
  .collection("customers").withConverter(dateConverter).add({
    ...customer,
    createdAt: now(),
    updatedAt: now(),
  });

const createOrUpdateCustomer = (id, customer) => {
  if (id) {
    return updateCustomer(id, customer);
  }
  return createCustomer(customer);
};

app.ports.saveCustomer.subscribe(async (customer) => {
  try {
    const { id: maybeId, ...body } = customer;

    const sanitizeCustomer = (body) => ({
      ...body,
      date: new Date(`${body.date} ${getTimezoneOffsetString()}`),
    });

    const sanitizedCustomer = sanitizeCustomer(body);
    const { id } = await createOrUpdateCustomer(maybeId, sanitizedCustomer);

    app.ports.saveSuccess.send({
      message: id,
    });
  } catch (e) {
    app.ports.saveError.send({
      message: e.message,
    });
  }
});

app.ports.loadCustomer.subscribe(async (id) => {
  try {
    const customer = await db.collection("customers").withConverter(dateConverter).doc(id).get();
    if (customer.exists) {
      const { id } = customer;

      const sanitizeCustomer = (body) => ({
        ...body,
        date: body.date.toISOString(),
      });

      const sanitizedCustomer = sanitizeCustomer(customer.data());

      app.ports.loadSuccess.send({
        id,
        ...sanitizedCustomer,
      });
    } else {
      throw new Error("Customer does not exist.");
    }
  } catch (e) {
    app.ports.loadError.send({
      message: e.message,
    });
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
