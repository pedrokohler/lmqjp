import "./main.css";
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

app.ports.saveCustomer.subscribe(async (customer) => {
  try {
    const { id, ...body } = customer;
    const sanitizeCustomer = (body) => ({
      ...body,
      date: new Date(`${body.date} ${getTimezoneOffsetString()}`),
    });
    const sanitizedCustomer = sanitizeCustomer(body);

    if (id) {
      await db.collection("customers").withConverter(dateConverter).doc(id).set(sanitizedCustomer);
    } else {
      await db.collection("customers").withConverter(dateConverter).add(sanitizedCustomer);
    }
  } catch (e) {
    console.error(e);
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
