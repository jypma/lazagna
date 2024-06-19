import { defineConfig } from "vite";
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";
import basicSsl from '@vitejs/plugin-basic-ssl';

export default defineConfig({
  plugins: [
    scalaJSPlugin({
      // path to the directory containing the sbt build
      // default: '.'
      cwd: '..',

      // sbt project ID from within the sbt build to get fast/fullLinkJS from
      // default: the root project of the sbt build
      projectID: 'client',

      // URI prefix of imports that this plugin catches (without the trailing ':')
      // default: 'scalajs' (so the plugin recognizes URIs starting with 'scalajs:')
      uriPrefix: 'scalajs',
    }),
    basicSsl({
      /** name of certification */
      name: 'test',
      /** custom trust domains */
      //domains: ['*.custom.com'],
      /** custom certification directory */
      //certDir: '/Users/.../.devServer/cert'
    })
  ],
  server: {
    host: "0.0.0.0",
    https: true,
    proxy: {
      '/drawings': {
        target: 'http://localhost:8080',
        changeOrigin: true,
        secure: false,
//        rewrite: (path) => path.replace(/^\/api/, ""),
        ws: true
      },
      '/user': {
        target: 'http://localhost:8080',
        changeOrigin: true,
        secure: false,
//        rewrite: (path) => path.replace(/^\/api/, ""),
        ws: true
      }
    }
  }
});
