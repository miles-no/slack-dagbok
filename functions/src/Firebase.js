
const functions = require("firebase-functions");
const admin = require("firebase-admin");


const db =
    admin.firestore();


exports.collTostring = collectionReference =>
    JSON.stringify(collectionReference)

exports.docTostring = docRef =>
    JSON.stringify(docRef)

exports.rootDoc = id =>
    db.doc(id)

exports.pathFromRef = ref => ref.path

exports.rootCollection = id =>
    db.collection(id)

exports.doc = id => collectionReference =>
    collectionReference.doc(id)

exports.addDocument = idField => data => collectionReference => {
    const ref = collectionReference.doc()
    const id = ref.id
    data[idField] = id
    return ref.set(data).then(() => id)
}
exports.setDocument = data => documentReference =>
    documentReference.set(data)

exports.mergeDocument = data => documentReference => {
    return documentReference.set(data, { merge: true })
}


exports.collectionGroup = id =>
    db.collectionGroup(id)

exports.doGet = onNotExists => onExists => documentReference =>
    documentReference
        .get()
        .then(doc => {
            if (doc.exists) return onExists(doc.data())
            else return onNotExists
        })

exports.doWhere = field => operator => value => collectionReference =>
    collectionReference
        .where(field, operator, value)

exports.doAndWhere = field => operator => value => query =>
    query
        .where(field, operator, value)

exports.doFind = query =>
    query
        .get()
        .then(querySnapshot => querySnapshot.docs.map(docSnapshot => docSnapshot.data()))

exports.findAll = collectionReference =>
    collectionReference
        .get()
        .then(querySnapshot => querySnapshot.docs.map(docSnapshot => docSnapshot.data()))

exports.findAllDocuments = collectionReference =>
    collectionReference
        .get()
        .then(querySnapshot => querySnapshot.docs.map(docSnapshot => docSnapshot.ref))

exports.collection = id => documentReference =>
    documentReference.collection(id)



exports.addToArray = path => value => documentReference =>
    documentReference
        .get()
        .then(doc => doc.data()).then(data => {

            const obj = {}
            obj[path] = admin.firestore.FieldValue.arrayUnion(value)
            return documentReference.update(obj)
        })

exports.removeFromArray = path => value => documentReference =>
    documentReference
        .get()
        .then(doc => doc.data()).then(data => {

            const obj = {}
            obj[path] = admin.firestore.FieldValue.arrayRemove(value)
            return documentReference.update(obj)
        })

exports.deleteDocument = documentReference =>
    documentReference.delete()

exports.doUpdateField = arrayOfPath => value => documentReference => {
    const path = arrayOfPath.join(".")
    const updateObject = {}
    updateObject[path] = value
    return documentReference.update(updateObject)
}

exports.doDeleteField = arrayOfPath => documentReference => {
    const path = arrayOfPath.join(".")
    const updateObject = {}
    updateObject[path] = admin.firestore.FieldValue.delete()
    return documentReference.update(updateObject)
}