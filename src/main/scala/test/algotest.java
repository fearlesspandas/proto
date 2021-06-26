//package test;
//
//import com.algorand.algosdk.account.Account;
//import com.algorand.algosdk.crypto.Address;
//import com.algorand.algosdk.kmd.client.ApiException;
//import com.algorand.algosdk.kmd.client.KmdClient;
//import com.algorand.algosdk.kmd.client.api.KmdApi;
//import com.algorand.algosdk.kmd.client.model.*;
//import com.algorand.algosdk.transaction.SignedTransaction;
//import com.algorand.algosdk.transaction.Transaction;
//import com.algorand.algosdk.util.Encoder;
//import com.algorand.algosdk.v2.client.common.AlgodClient;
//import com.algorand.algosdk.v2.client.common.IndexerClient;
//import com.algorand.algosdk.v2.client.common.Response;
//import com.algorand.algosdk.v2.client.model.PendingTransactionResponse;
//import com.algorand.algosdk.v2.client.model.PostTransactionsResponse;
//import com.algorand.algosdk.v2.client.model.TransactionsResponse;
//
//import java.io.IOException;
//import java.security.NoSuchAlgorithmException;
//import java.util.ArrayList;
//import java.util.Arrays;
//import java.util.List;
//
//public class Main {
//    private static String token = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
//    private static KmdApi kmd = null;
//
//    public static void main(String[] args) throws Exception {
//        // Initialize algod/indexer v2 clients.
//        AlgodClient algod = new AlgodClient("http://localhost", 4001, token);
//        IndexerClient indexer = new IndexerClient("http://localhost", 8980);
//
//        // Initialize KMD v1 client
//        KmdClient kmdClient = new KmdClient();
//        kmdClient.setBasePath("http://localhost:4002");
//        kmdClient.setApiKey(token);
//        kmd = new KmdApi(kmdClient);
//
//        // Get accounts from sandbox.
//        String walletHandle = getDefaultWalletHandle();
//        List<Address> accounts  = getWalletAccounts(walletHandle);
//
//        // Create a payment transaction
//        Transaction tx1 = Transaction.PaymentTransactionBuilder()
//                .lookupParams(algod) // lookup fee, firstValid, lastValid
//                .sender(accounts.get(0))
//                .receiver(accounts.get(1))
//                .amount(1000000)
//                .noteUTF8("test transaction!")
//                .build();
//
//        // Sign with KMD
//        SignedTransaction stx1a = signTransactionWithKMD(tx1, walletHandle);
//        byte[] stx1aBytes = Encoder.encodeToMsgPack(stx1a);
//
//        // Sign with private key
//        byte[] privateKey = lookupPrivateKey(accounts.get(0), walletHandle);
//        Account account = new Account(privateKey);
//        SignedTransaction stx1b = account.signTransaction(tx1);
//        byte[] stx1bBytes = Encoder.encodeToMsgPack(stx1b);
//
//        // KMD and signing directly should both be the same.
//        if (!Arrays.equals(stx1aBytes, stx1bBytes)) {
//            throw new RuntimeException("KMD disagrees with the manual signature!");
//        }
//
//        // Send transaction
//        Response<PostTransactionsResponse> post = algod.RawTransaction().rawtxn(stx1aBytes).execute();
//        if (!post.isSuccessful()) {
//            throw new RuntimeException("Failed to post transaction");
//        }
//
//        // Wait for confirmation
//        boolean done = false;
//        while (!done) {
//            Response<PendingTransactionResponse> txInfo = algod.PendingTransactionInformation(post.body().txId).execute();
//            if (!txInfo.isSuccessful()) {
//                throw new RuntimeException("Failed to check on tx progress");
//            }
//            if (txInfo.body().confirmedRound != null) {
//                done = true;
//            }
//        }
//
//        // Wait for indexer to index the round.
//        Thread.sleep(5000);
//
//        // Query indexer for the transaction
//        Response<TransactionsResponse> transactions = indexer.searchForTransactions()
//                .txid(post.body().txId)
//                .execute();
//
//        if (!transactions.isSuccessful()) {
//            throw new RuntimeException("Failed to lookup transaction");
//        }
//
//        System.out.println("Transaction received! \n" + transactions.toString());
//    }
//
//    public static SignedTransaction signTransactionWithKMD(Transaction tx, String walletHandle) throws IOException, ApiException {
//        SignTransactionRequest req = new SignTransactionRequest();
//        req.transaction(Encoder.encodeToMsgPack(tx));
//        req.setWalletHandleToken(walletHandle);
//        req.setWalletPassword("");
//        byte[] stxBytes = kmd.signTransaction(req).getSignedTransaction();
//        return Encoder.decodeFromMsgPack(stxBytes, SignedTransaction.class);
//    }
//
//    public static byte[] lookupPrivateKey(Address addr, String walletHandle) throws ApiException {
//        ExportKeyRequest req = new ExportKeyRequest();
//        req.setAddress(addr.toString());
//        req.setWalletHandleToken(walletHandle);
//        req.setWalletPassword("");
//        return kmd.exportKey(req).getPrivateKey();
//    }
//
//    public static String getDefaultWalletHandle() throws ApiException {
//        for (APIV1Wallet w : kmd.listWallets().getWallets()) {
//            if (w.getName().equals("unencrypted-default-wallet")) {
//                InitWalletHandleTokenRequest tokenreq = new InitWalletHandleTokenRequest();
//                tokenreq.setWalletId(w.getId());
//                tokenreq.setWalletPassword("");
//                return kmd.initWalletHandleToken(tokenreq).getWalletHandleToken();
//            }
//        }
//        throw new RuntimeException("Default wallet not found.");
//    }
//
//    public static List<Address> getWalletAccounts(String walletHandle) throws ApiException, NoSuchAlgorithmException {
//        List<Address> accounts = new ArrayList<>();
//
//        ListKeysRequest keysRequest = new ListKeysRequest();
//        keysRequest.setWalletHandleToken(walletHandle);
//        for (String addr : kmd.listKeysInWallet(keysRequest).getAddresses()) {
//            accounts.add(new Address(addr));
//        }
//
//        return accounts;
//    }
//}
